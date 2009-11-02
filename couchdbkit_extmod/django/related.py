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

RELATED_VIEW_TEMPLATE = """function (doc) {
	if (doc.doc_type == "%s") emit([doc.%s, doc.%s], doc);
}"""

class ForeignRelatedObjectsDescriptor(object):
    def __init__(self, host_field, host_class, related_field, related_class, related_manager = None):
        if related_manager == None:
            self.related_manager = RelatedManager()
        else:
            self.related_manager = related_manager
        self.related_manager.associate(host_field, host_class, related_field, related_class)
    
    def __get__(self, instance, instance_type=None):
        if instance is None:
            return self
        
        return self.related_manager.bind_instance(instance)

class RelatedManager(object):
    filters = []

    def __init__(self, filters=[]):
        self.filters = self.filters + filters
        self.host_field = None # Filled in by descriptor
        self.host_class = None
        self.related_field = None
        self.related_class = None
        self.app_label = ""
        self.instance = None # Filled in on binding
        self.view = None
    
    def associate(self, host_field, host_class, related_field, related_class):
        self.host_field = host_field
        self.host_class = host_class
        self.related_field = related_field
        self.related_class = related_class
        document_module = sys.modules[host_class.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
    
    def bind_instance(self, instance):
        """
        Return a clone which is bound to an instance.
        """
        c = self.__class__()
        
        c.filters = self.filters # No deep copy here (!)
        c.host_field, c.host_class = self.host_field, self.host_class
        c.related_field, c.related_class = self.related_field, self.related_class
        c.app_label = self.app_label
        c.instance = instance
        view_name = "%s/%s_%s" % (self.app_label, c.host_class.__name__, c.host_field)
        c.view = self.related_class.view(view_name, key=instance._id)
        return c
    
    def _generate_views(self):
        views = {}
        for filter in self.filters:
            view_name = "%s_%s_by_%s" % (self.host_class.__name__, self.host_field, filter)
            views[view_name] = {"map": RELATED_VIEW_TEMPLATE % (self.related_class.__name__, self.related_field, filter)}
        return views
    
    def add(self, *objs):
        for obj in objs:
            if not isinstance(obj, self.related_class):
                raise TypeError, "'%s' instance expected" % self.related_class.__name__
            setattr(obj, self.related_field, self.instance)
            obj.save()
    
    def remove(self, *objs):
        i = self.instance._id
        for obj in objs:
            if getattr(obj, self.related_field)._id == i:
                setattr(obj, self.related_field, None)
                obj.save()
            else:
                raise ValueError("%r is not related to %r." % (obj, self.instance))
    
    def clear(self):
        for obj in self.all():
            setattr(obj, self.related_field, None)
            obj.save()
    
    def create(self, **kwargs):
        kwargs.update({self.related_field: self.instance})
        return self.related_class(**kwargs)
    
    def all(self):
        return list(self.view)
    
    def count(self):
        return self.view.count()
    
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
        
        view_name = "%s/%s_%s_by_%s" % (self.app_label, self.host_class.__name__, self.host_field, lookup_field)
        
        if not lookup_field in self.filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            return self.related_class.temp_view({"map": RELATED_VIEW_TEMPLATE % (self.related_class.__name__, self.related_field, lookup_field)}, **view_params)
        else:
            return self.related_class.view(view_name, **view_params)
