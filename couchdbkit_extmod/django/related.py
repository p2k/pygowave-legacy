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

import sys
from couchdbkit import schema
from couchdbkit_extmod.django.loading import get_schema, SIMPLE_VIEW_TEMPLATE
from django.db.models.fields.related import RECURSIVE_RELATIONSHIP_CONSTANT

RELATED_VIEW_TEMPLATE = """function (doc) {
	if (doc.doc_type == "%s") emit([doc.%s, doc.%s], doc);
}"""

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
        
        try:
            getattr(related_schema, "_properties")
        except AttributeError:
            assert isinstance(related_schema, basestring), "%s(%r) is invalid. First parameter to ForeignKey must be either a model, a model name, or the string %r" % (self.__class__.__name__, related_schema, RECURSIVE_RELATIONSHIP_CONSTANT)
        
        self._related_schema = related_schema
        self._related_name = related_name
        self._related_manager = related_manager
        self._host_schema = None # Filled in by metaclass
    
    def _generate_views(self):
        views = {}
        view_name = "%s_%s" % (self._related_schema.__name__, self._related_name)
        views[view_name] = {"map": SIMPLE_VIEW_TEMPLATE % (self._host_schema.__name__, self.name)}
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
    
        try:
            getattr(value, "_properties")
        except AttributeError:
            raise BadValueError(
                'Property %s must be DocumentSchema instance, not a %s' % (self.name,
                type(value).__name__))
    
        return value
    
    def to_python(self, value):
        return self._related_schema.get(value)
    
    def to_json(self, value):
        if value == None:
            return None
        
        try:
            getattr(value, "_properties")
        except AttributeError:
            schema = self._related_schema()
            if not isinstance(value, dict):
                raise BadValueError("%s is not a dict" % str(value))
            value = schema(**value)
        
        if not value._id:
            value.save()
        return value._id

class ForeignRelatedObjectsDescriptor(object):
    def __init__(self, prop):
        if prop._related_manager == None:
            prop._related_manager = RelatedManager()
        prop._related_manager.associate(prop)
        self.prop = prop
    
    def __get__(self, instance, instance_type=None):
        if instance is None:
            return self
        
        return self.prop._related_manager.bind_instance(instance)

class RelatedManager(object):
    filters = []
    
    def __init__(self, filters=[]):
        self.filters = self.filters + filters
        self.prop = None # Filled in by descriptor
        self.app_label = ""
        self.instance = None # Filled in on binding
        self.view = None
    
    def associate(self, prop):
        self.prop = prop
        document_module = sys.modules[prop._host_schema.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
    
    def bind_instance(self, instance):
        """
        Return a clone which is bound to an instance.
        """
        c = self.__class__()
        c.filters = self.filters # No deep copy here (!)
        c.associate(self.prop)
        c.instance = instance
        view_name = "%s/%s_%s" % (self.app_label, self.prop._related_schema.__name__, self.prop._related_name)
        c.view = self.prop._host_schema.view(view_name, key=instance._id)
        return c
    
    def _generate_views(self):
        views = {}
        for filter in self.filters:
            view_name = "%s_%s_by_%s" % (self.prop._related_schema.__name__, self.prop._related_name, filter)
            views[view_name] = {"map": RELATED_VIEW_TEMPLATE % (self.prop._host_schema.__name__, self.prop.name, filter)}
        return views
    
    def add(self, *objs):
        for obj in objs:
            if not isinstance(obj, self.prop._related_schema):
                raise TypeError, "'%s' instance expected" % self.prop._related_schema.__name__
            setattr(obj, self.prop._related_name, self.instance)
            obj.save()
    
    def remove(self, *objs):
        i = self.instance._id
        for obj in objs:
            if getattr(obj, self.prop._related_name)._id == i:
                setattr(obj, self.prop._related_name, None)
                obj.save()
            else:
                raise ValueError("%r is not related to %r." % (obj, self.instance))
    
    def clear(self):
        for obj in self.all():
            setattr(obj, self.prop._related_name, None)
            obj.save()
    
    def create(self, **kwargs):
        kwargs.update({self.prop._related_name: self.instance})
        return self.prop._related_schema(**kwargs)
    
    def all(self):
        return list(self.view)
    
    def count(self):
        return self.view.count()
    
    def get(self, **kwargs):
        return self.filter(**kwargs).one()
    
    def filter(self, **kwargs):
        lookup_field = ""
        fcount = 0
        
        view_params = {}
        
        for arg_name, arg_val in kwargs.iteritems():
            if fcount > 0:
                raise ValueError, "Filters with more than one field are currently not supported"
            
            spl = arg_name.split("__")
            lookup_field = spl[0]
            
            if len(spl) > 2:
                raise ValueError, "Document-spanning queries are currently not supported"
            
            if len(spl) > 1:
                if spl[1] not in ("eq", "neq", "gt", "lt", "gte", "lte"):
                    raise ValueError, "Document-spanning queries are currently not supported"
                if spl[1] == "eq":
                    view_params["key"] = [self.instance._id, arg_val]
                elif spl[1] == "gte":
                    view_params["startkey"] = [self.instance._id, arg_val]
                elif spl[1] == "lte":
                    view_params["endkey"] = [self.instance._id, arg_val]
                else:
                    raise ValueError, "Query modification '%s' not supported" % spl[1]
            else:
                view_params["key"] = [self.instance._id, arg_val]
            
            fcount += 1
        
        if fcount == 0:
            return self.view
        
        view_name = "%s/%s_%s_by_%s" % (self.app_label, self.prop._related_schema.__name__, self.prop._related_name, lookup_field)
        
        if not lookup_field in self.filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            design = {"map": RELATED_VIEW_TEMPLATE % (self.prop._host_schema.__name__, self.prop.name, lookup_field)}
            return self.prop._host_schema.temp_view(design, **view_params)
        else:
            return self.prop._host_schema.view(view_name, **view_params)
