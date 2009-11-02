# -*- coding: utf-8 -*-
#
# Copyright 2009 Patrick Schneider <patrick.p2k.schneider@gmail.com>
#
# Original software:
# Copyright (c) 2008-2009 Benoit Chesneau <benoitc@e-engura.com> 
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

""" Wrapper of couchdbkit Document and Properties for django. It also 
add possibility to a document to register itself in CouchdbkitHandler
"""

import sys

from couchdbkit import schema
from couchdbkit.exceptions import BadValueError
from couchdbkit_extmod.django.loading import get_schema, register_schema, SIMPLE_VIEW_TEMPLATE

from manager import DocumentManager
from related import ForeignRelatedObjectsDescriptor

__all__ = ['Property', 'StringProperty', 'IntegerProperty', 
            'DecimalProperty', 'BooleanProperty', 'FloatProperty', 
            'DateTimeProperty', 'DateProperty', 'TimeProperty', 
            'dict_to_json', 'list_to_json', 'value_to_json', 
            'value_to_python', 'dict_to_python', 'list_to_python', 
            'convert_property', 'DocumentSchema', 'Document', 
            'SchemaProperty', 'ListProperty', 'DictProperty',
            'StringListProperty', 'ForeignKeyProperty']

class DocumentMeta(schema.SchemaProperties):
    def __new__(cls, name, bases, attrs):
        super_new = super(DocumentMeta, cls).__new__
        parents = [b for b in bases if isinstance(b, DocumentMeta)]
        if not parents:
            return super_new(cls, name, bases, attrs)
        
        new_class = super_new(cls, name, bases, attrs)
        document_module = sys.modules[new_class.__module__]
        app_label = document_module.__name__.split('.')[-2]
        register_schema(app_label, new_class)
        
        new_class = get_schema(app_label, name)
        
        # Inject views for ForeignKeyProperty
        for prop_name, prop in new_class._properties.iteritems():
            if isinstance(prop, ForeignKeyProperty):
                prop._my_schema = new_class
                if prop._related_name != None:
                    setattr(prop._related_schema, prop._related_name, ForeignRelatedObjectsDescriptor(prop._related_name, prop._related_schema, prop_name, new_class, prop._related_manager))
        
        # Process manager (quick and dirty)
        if attrs.has_key("objects"):
            new_class.objects.contribute_to_class(new_class, "objects")
        else:
            new_class.objects._copy_to_class(new_class, "objects")
        
        return new_class

class Document(schema.Document):
    """ Document object for django extension """
    __metaclass__ = DocumentMeta
    
    get_id = property(lambda self: self['_id'])
    get_rev = property(lambda self: self['_rev'])
    
    objects = DocumentManager()
    
DocumentSchema = schema.DocumentSchema    

#  properties
Property = schema.Property
StringProperty = schema.StringProperty
IntegerProperty = schema.IntegerProperty
DecimalProperty = schema.DecimalProperty
BooleanProperty = schema.BooleanProperty
FloatProperty = schema.FloatProperty
DateTimeProperty = schema.DateTimeProperty
DateProperty = schema.DateProperty
TimeProperty = schema.TimeProperty
SchemaProperty = schema.SchemaProperty
ListProperty = schema.ListProperty
DictProperty = schema.DictProperty
StringListProperty = schema.StringListProperty

class ForeignKeyProperty(schema.Property):
    """
    This property allows to have a reference to an external Document.
    It returns a schema.Document object on demand.
    The behaviour is similar to SchemaProperty, except that a reference is
    stored instead of the real document. Additionally, a back-reference view is
    stored in the linked class if related_name is specified.
    """
    
    def __init__(self, related_schema, verbose_name=None, name=None, required=False, validators=None, default=None, related_name=None, related_manager=None):
        super(ForeignKeyProperty, self).__init__(verbose_name=verbose_name, name=name, required=required, validators=validators)
        
        if not issubclass(related_schema, DocumentSchema):
            raise TypeError('schema should be a DocumentSchema subclass')
        
        self._related_schema = related_schema
        self._related_name = related_name
        self._related_manager = related_manager
        self._my_schema = None # Filled in by metaclass
    
    def _generate_views(self):
        views = {}
        view_name = "%s_%s" % (self._related_schema.__name__, self._related_name)
        views[view_name] = {"map": SIMPLE_VIEW_TEMPLATE % (self._my_schema.__name__, self.name)}
        if self._related_manager != None:
            views.update(self._related_manager._generate_views())
        return views
    
    def default_value(self):
        return None
    
    def empty(self, value):
        if value == None:
            return True
        return False
    
    def validate(self, value, required=True):
        if value == None:
            return None
        value.validate(required=required)
        value = super(ForeignKeyProperty, self).validate(value)
        
        if value == None:
            return value
    
        if not isinstance(value, DocumentSchema):
            raise BadValueError(
                'Property %s must be DocumentSchema instance, not a %s' % (self.name,
                type(value).__name__))
    
        return value
    
    def to_python(self, value):
        return self._related_schema.get(value)
    
    def to_json(self, value):
        if value == None:
            return None
        
        if not isinstance(value, DocumentSchema):
            schema = self._related_schema()
    
            if not isinstance(value, dict):
                raise BadValueError("%s is not a dict" % str(value))
            
            value = schema(**value)
        if not value._id:
            value.save()
        return value._id

# some utilities
dict_to_json = schema.dict_to_json
list_to_json = schema.list_to_json
value_to_json = schema.value_to_json
value_to_python = schema.value_to_python
dict_to_python = schema.dict_to_python
list_to_python = schema.list_to_python
convert_property = schema.convert_property
