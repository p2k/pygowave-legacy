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
from couchdbkit_extmod.django.loading import get_schema
from couchdbkit_extmod.django.manager import DocumentManager, Filter
from django.db.models.fields.related import RECURSIVE_RELATIONSHIP_CONSTANT
from django.core.exceptions import ObjectDoesNotExist

class RelatedProperty(schema.Property):
    """
    This is the base property for all external reference properties to Documents.
    """
    
    def __init__(self, related_schema, verbose_name=None, name=None, required=False, validators=None, default=None, related_name=None, related_filters=[]):
        super(RelatedProperty, self).__init__(verbose_name=verbose_name, name=name, required=required, validators=validators)
        
        try:
            getattr(related_schema, "_properties")
        except AttributeError:
            assert isinstance(related_schema, basestring), "%s(%r) is invalid. First parameter to a RelatedProperty must be either a model, a model name, or the string %r" % (self.__class__.__name__, related_schema, RECURSIVE_RELATIONSHIP_CONSTANT)
        
        self._related_schema = related_schema
        self._related_name = related_name
        self._related_filters = Filter.toFilters(related_filters)
        self._host_schema = None # Filled in by metaclass
        self._subclasses = []
    
    def class_names(self):
        if len(self._subclasses) == 0:
            return self._host_schema.__name__
        else:
            return map(lambda c: c.__name__, [self._host_schema] + self._subclasses)
    
    def contribute_to_class(self, related_schema, related_name):
        raise NotImplementedError
    
    def generate_views(self):
        raise NotImplementedError

class ForeignKeyProperty(RelatedProperty):
    """
    This property allows to have a reference to an external Document.
    It returns a schema.Document object on demand.
    The behaviour is similar to SchemaProperty, except that a reference is
    stored instead of the real document. Additionally, a back-reference view is
    stored in the linked class if related_name is specified.
    """
    
    def generate_views(self):
        views = {}
        view_name = "%s_%s" % (self._related_schema.__name__, self._related_name)
        views[view_name] = DocumentManager._generate_view(self.class_names(), self.name)
        for filter in self._related_filters:
            view_name = "%s_%s_by_%s" % (self._related_schema.__name__, self._related_name, filter.name())
            views[view_name] = DocumentManager._generate_view(self.class_names(), [self.name, filter])
        return views
    
    def contribute_to_class(self, related_schema, related_name):
        setattr(related_schema, related_name, ForeignRelatedObjectsDescriptor(self))
    
    def create_related_manager(self, instance):
        return ForeignRelatedManager(self, instance)
    
    def validate(self, value, required=True):
        return value # Do not validate
    
    def empty(self, value):
        if value == None:
            return True
        return False
    
    def to_python(self, value):
        return value
    
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
    
    def __property_init__(self, document_instance, value):
        if isinstance(value, basestring):
            document_instance._doc[self.name] = value
            return
        else:
            super(ForeignKeyProperty, self).__property_init__(document_instance, value)
    
    def __get__(self, instance, instance_type=None):
        if instance == None:
            return self
        
        value = instance._doc.get(self.name)
        if value is not None:
            value = self._related_schema.get(value)
        
        return value

class ManyToManyProperty(RelatedProperty):
    """
    This property allows to have multiple reference to an external Document.
    It returns a list of schema.Document objects on demand.
    The behaviour is similar to SchemaProperty, except that a list of references is
    stored instead of the real documents. Additionally, a back-reference view is
    stored in the linked class if related_name is specified.
    """
    
    def __init__(self, related_schema, verbose_name=None, name=None, required=False, validators=None, default=None, related_name=None, related_filters=[], local_filters=[]):
        super(ManyToManyProperty, self).__init__(related_schema, verbose_name, name, required, validators, default, related_name, related_filters)
        self._local_filters = Filter.toFilters(local_filters)
    
    def __property_init__(self, document_instance, value):
        if isinstance(value, list):
            document_instance._doc[self.name] = value
            return
        if isinstance(value, ReverseRelatedManager):
            value._bind(document_instance)
        super(ManyToManyProperty, self).__property_init__(document_instance, value)
    
    def validate(self, value, required=True):
        return value # Do not validate
    
    def contribute_to_class(self, related_schema, related_name):
        prop = ManyToManyProperty(self._host_schema, related_name=self.name, related_filters=self._local_filters, local_filters=self._related_filters)
        prop._host_schema = related_schema
        prop.__property_config__(related_schema, related_name)
        setattr(related_schema, related_name, prop)
        related_schema._properties[related_name] = prop
    
    def generate_views(self):
        views = {}
        view_name = "%s_%s" % (self._host_schema.__name__, self.name)
        releated_prop = getattr(self._related_schema, self._related_name)
        views[view_name] = DocumentManager._generate_view(releated_prop.class_names(), self._related_name, True)
        for filter in self._local_filters:
            view_name = "%s_%s_by_%s" % (self._host_schema.__name__, self.name, filter.name())
            views[view_name] = DocumentManager._generate_view(releated_prop.class_names(), [self._related_name, filter], True)
        return views
    
    def default_value(self):
        value = super(ManyToManyProperty, self).default_value()
        if value is None:
            value = []
        return list(value)
    
    def empty(self, value):
        return value.count() == 0
    
    def __get__(self, instance, instance_type=None):
        if instance == None:
            return self
        return ReverseRelatedManager(self, instance)
    
    def to_python(self, value):
        return value
    
    def to_json(self, values):
        if isinstance(values, ReverseRelatedManager):
            return values.instance._doc.get(self.name)
        
        if len(values) == 0:
            return []
        
        for i in xrange(len(values)):
            try:
                getattr(values[i], "_properties")
            except AttributeError:
                schema = self._related_schema()
                if not isinstance(values[i], dict):
                    raise BadValueError("%s is not a dict" % str(value))
                values[i] = schema(**values[i])
            if not values[i]._id:
                values[i].save()
        
        return map(lambda o: o._id, values)

class ForeignRelatedObjectsDescriptor(object):
    def __init__(self, prop):
        self.prop = prop
    
    def __get__(self, instance, instance_type=None):
        if instance is None:
            return self
        
        return self.prop.create_related_manager(instance)

class RelatedManager(object):
    def add(self, *objs):
        raise NotImplementedError
    def remove(self, *objs):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError
    def create(self, **kwargs):
        raise NotImplementedError
    def all(self):
        raise NotImplementedError
    def count(self):
        raise NotImplementedError
    def get(self, **kwargs):
        ret = self.filter(**kwargs).one()
        if ret == None:
            raise ObjectDoesNotExist
        return ret
    def filter(self, **kwargs):
        raise NotImplementedError

class ForeignRelatedManager(RelatedManager):
    def __init__(self, prop, instance):
        self.prop = prop
        document_module = sys.modules[prop._host_schema.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
        self.instance = instance
        view_name = "%s/%s_%s" % (self.app_label, self.prop._related_schema.__name__, self.prop._related_name)
        self.view = self.prop._host_schema.view(view_name, key=instance._id)
    
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
    
    def filter(self, **kwargs):
        lookup_field, view_params = DocumentManager._process_filter_args(kwargs, self.instance._id)
        
        if lookup_field == None:
            return self.view
        
        view_name = "%s/%s_%s_by_%s" % (self.app_label, self.prop._related_schema.__name__, self.prop._related_name, lookup_field.name())
        
        if not lookup_field in self.prop._related_filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            design = DocumentManager._generate_view(self.prop._host_schema.__name__, [self.prop.name, lookup_field])
            return self.prop._host_schema.temp_view(design, **view_params)
        else:
            return self.prop._host_schema.view(view_name, **view_params)

class ReverseRelatedManager(RelatedManager):
    def __init__(self, prop, instance):
        self.prop = prop
        document_module = sys.modules[prop._host_schema.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
        if instance != None:
            self._bind(instance)
    
    def _bind(self, instance):
        self.instance = instance
        self.values = instance._doc.get(self.prop.name)
        view_name = "%s/%s_%s" % (self.app_label, self.prop._host_schema.__name__, self.prop.name)
        self.view = self.prop._related_schema.view(view_name, key=instance._id)
    
    def add(self, *objs):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        for obj in objs:
            if not isinstance(obj, self.prop._related_schema):
                raise TypeError, "'%s' instance expected" % self.prop._related_schema.__name__
            if obj._id not in self.values:
                self.values.append(obj._id)
            obj_values = getattr(obj, self.prop._related_name).values
            if self.instance._id not in obj_values:
                obj_values.append(self.instance._id)
                obj.save()
        self.instance.save()
    
    def remove(self, *objs):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        for obj in objs:
            if not isinstance(obj, self.prop._related_schema):
                raise TypeError, "'%s' instance expected" % self.prop._related_schema.__name__
            if obj._id not in self.values:
                raise ValueError("%r is not related to %r." % (obj, self.instance))
            self.values.remove(obj._id)
            getattr(obj, self.prop._related_name).values.remove(self.instance._id)
            obj.save()
        self.instance.save()
    
    def clear(self):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        for obj in self.all():
            getattr(obj, self.prop._related_name).values.remove(self.instance._id)
            obj.save()
        del self.values[:]
        self.instance.save()
    
    def create(self, **kwargs):
        obj = self.prop._related_schema(**kwargs)
        obj.save()
        self.add(obj)
        return obj
    
    def all(self):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        return list(self.view)
    
    def count(self):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        return len(self.values)
    
    def filter(self, **kwargs):
        assert self.instance != None, "Cannot perform operations on an unbound manager"
        lookup_field, view_params = DocumentManager._process_filter_args(kwargs, self.instance._id)
        
        if lookup_field == None:
            return self.view
        
        view_name = "%s/%s_%s_by_%s" % (self.app_label, self.prop._host_schema.__name__, self.prop.name, lookup_field.name())
        
        if not lookup_field in self.prop._local_filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            design = DocumentManager._generate_view(self.prop._related_schema.__name__, [self.prop._related_name, lookup_field], True)
            return self.prop._related_schema.temp_view(design, **view_params)
        else:
            return self.prop._related_schema.view(view_name, **view_params)
