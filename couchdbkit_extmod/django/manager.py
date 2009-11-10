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

import sys, datetime

from django.core.exceptions import ObjectDoesNotExist

__all__ = ["DocumentManager"]

class Filter(object):
    def __init__(self, field_descriptor, **options):
        spl = field_descriptor.split("__")
        self.field_descriptor = spl[0]
        if len(spl) > 1:
            self.modifier = spl[1]
        else:
            self.modifier = None
        self.options = options
    
    def __eq__(self, other):
        if isinstance(other, Filter):
            return self.field_descriptor == other.field_descriptor and self.modifier == other.modifier
        elif isinstance(other, basestring):
            return self.name() == other
        return False
    
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def __str__(self):
        return self.field_descriptor
    
    def __repr__(self):
        if len(self.options) > 0:
            opts = ", ".join(map(lambda key: "%s=%s" % (key, repr(self.options[key])), self.options.keys()))
            return "Filter(%s, %s)" % (repr(self.name()), opts)
        else:
            return "Filter(%s)" % repr(self.name())
    
    def name(self):
        if self.modifier != None:
            return "%s__%s" % (self.field_descriptor, self.modifier)
        else:
            return self.field_descriptor
    
    @classmethod
    def toFilters(cls, lst):
        ret = []
        for item in lst:
            if isinstance(item, cls):
                ret.append(item)
            else:
                ret.append(cls(item))
        return ret

class DocumentManager(object):
    filters = []

    def __init__(self, filters=[]):
        self.filters = Filter.toFilters(self.filters + filters)

    @classmethod
    def copy_to_class(cls, doc, name):
        thecopy = cls()
        thecopy.contribute_to_class(doc, name)
    
    def contribute_to_class(self, doc, name):
        self.doc = doc
        document_module = sys.modules[doc.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
        setattr(doc, name, DocumentManagerDescriptor(self))
        if name == "objects":
            setattr(doc, "_default_manager", DocumentManagerDescriptor(self))
        # Prepare all view
        if not "all" in self.filters:
            self._all_view = None
        else:
            self._all_view = self.view("%s/%s_all" % (self.app_label, self.doc.__name__))
    
    def create(self, **kwargs):
        return self.doc(**kwargs)
    
    def get(self, _id=None, **kwargs):
        if _id != None:
            ret = self.doc.get(_id)
        elif len(kwargs) == 1 and kwargs.has_key("pk"): # Compatibility hack
            ret = self.doc.get(kwargs["pk"])
        else:
            ret = self.filter(**kwargs).one()
        if ret == None:
            raise ObjectDoesNotExist
        return ret
    
    def all(self):
        if self._all_view == None:
            print "Warning: Temp View used for lookup '%s/%s_all'" % (self.app_label, self.doc.__name__)
            self._all_view = self.temp_view(self._generate_view(self.doc.__name__, "_id"))
        return list(self._all_view)
    
    def view(self, view_name, wrapper=None, dynamic_properties=True, **params):
        return self.doc.view(view_name, wrapper=wrapper, dynamic_properties=dynamic_properties, **params)
    
    def temp_view(self, design, wrapper=None, dynamic_properties=True, **params):
        return self.doc.temp_view(design, wrapper=wrapper, dynamic_properties=dynamic_properties, **params)
    
    def generate_views(self):
        views = {}
        for filter in self.filters:
            if filter.name() == "all":
                view_name = "%s_all" % (self.doc.__name__)
                views[view_name] = self._generate_view(self.doc.__name__, "_id")
            else:
                view_name = "%s_by_%s" % (self.doc.__name__, filter.name())
                views[view_name] = self._generate_view(self.doc.__name__, filter)
        return views
    
    @staticmethod
    def _generate_view(doc_type, fields, m2m=False):
        if isinstance(fields, basestring) or isinstance(fields, Filter):
            fields = [fields]
        
        if isinstance(doc_type, basestring):
            doc_type = "doc.doc_type == \"%s\"" % doc_type
        else:
            doc_type = " || ".join(map(lambda s: "doc.doc_type == \"%s\"" % s, doc_type))
        
        if not isinstance(fields[-1], Filter) or fields[-1].modifier != "icontains":
            if not m2m:
                if len(fields) == 1:
                    sfields = "doc." + str(fields[0])
                else:
                    sfields = "[" + ", ".join(map(lambda s: "doc."+str(s), fields)) + "]"
                return {"map":
"""function (doc) {
    if (%(doc_type)s)
        emit(%(fields)s, doc);
}""" % {"doc_type": doc_type, "fields": sfields}}
            
            else:
                list_field = fields[0]
                fields = list(fields)
                fields[0] = "%s[i]" % list_field
                if len(fields) == 1:
                    sfields = "doc." + str(fields[0])
                else:
                    sfields = "[" + ", ".join(map(lambda s: "doc."+str(s), fields)) + "]"
                return {"map":
"""function(doc) {
    if (%(doc_type)s) {
        for (var i in doc.%(list_field)s)
            emit(%(fields)s, doc);
    }
}""" % {"doc_type": doc_type, "list_field": list_field, "fields": sfields}}
        
        else:
            
            min_len = fields[-1].options.get("min_len", 0)
            if min_len <= 1:
                min_len = ""
            else:
                min_len = "-%d" % (min_len-1)
            
            if not m2m:
                if len(fields) == 1:
                    search_field = fields[0]
                    sfields = "s.substring(p)"
                else:
                    search_field = fields[-1]
                    sfields = "[" + ", ".join(map(lambda s: "doc."+str(s), fields[:-1])) + ", s.substring(p)]"
                
                return {"map":
"""function (doc) {
    if (%(doc_type)s) {
        var s = doc.%(search_field)s;
        for (var p = 0; p < s.length%(min_len)s; p++) {
            if (s.indexOf(s.substring(p)) == p)
                emit(%(fields)s, doc);
        }
    }
}""" % {"doc_type": doc_type, "search_field": search_field, "min_len": min_len, "fields": sfields}}
            
            else:
                list_field = fields[0]
                fields = list(fields)
                fields[0] = "%s[i]" % list_field
                if len(fields) == 1:
                    search_field = fields[0]
                    sfields = "s.substring(p)"
                else:
                    search_field = fields[-1]
                    sfields = "[" + ", ".join(map(lambda s: "doc."+str(s), fields[:-1])) + ", s.substring(p)]"
                return {"map":
"""function(doc) {
    if (%(doc_type)s) {
        var s = doc.%(search_field)s;
        for (var p = 0; p < s.length%(min_len)s; p++) {
            if (s.indexOf(s.substring(p)) == p) {
                for (var i in doc.%(list_field)s)
                    emit(%(fields)s, doc);
            }
        }
    }
}""" % {"doc_type": doc_type, "search_field": search_field, "min_len": min_len, "list_field": list_field, "fields": sfields}}
    
    def filter(self, **kwargs):
        lookup_field, view_params = self._process_filter_args(kwargs)
        
        if lookup_field == None:
            if self._all_view == None:
                print "Warning: Temp View used for lookup '%s/%s_all'" % (self.app_label, self.doc.__name__)
                self._all_view = self.temp_view(self._generate_view(self.doc.__name__, "_id"))
            return self._all_view
        
        view_name = "%s/%s_by_%s" % (self.app_label, self.doc.__name__, lookup_field.name())
        
        if not lookup_field in self.filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            design = self._generate_view(self.doc.__name__, lookup_field)
            print design["map"], view_params, repr(lookup_field)
            return self.temp_view(design, **view_params)
        else:
            return self.view(view_name, **view_params)
    
    @staticmethod
    def _process_filter_args(filter_args, fkey=None):
        lookup_field = ""
        fcount = 0
        
        view_params = {}
        
        for arg_name, arg_val in filter_args.iteritems():
            if fcount > 0:
                raise ValueError, "Filters with more than one field are currently not supported"
            
            lookup_field = Filter(arg_name)
            
            if isinstance(arg_val, datetime.datetime):
                arg_val = arg_val.replace(microsecond=0).isoformat() + 'Z'
            
            if lookup_field.modifier != None:
                if lookup_field.modifier not in ("eq", "neq", "gt", "lt", "gte", "lte", "contains", "icontains"):
                    raise ValueError, "Document-spanning queries are currently not supported"
                if lookup_field.modifier == "eq":
                    lookup_field.modifier = None
                    if fkey == None:
                        view_params["key"] = arg_val
                    else:
                        view_params["key"] = [fkey, arg_val]
                elif lookup_field.modifier == "icontains":
                    if fkey == None:
                        view_params["startkey"] = arg_val.lower()
                        view_params["endkey"] = arg_val.lower() + u"\u9999"
                    else:
                        view_params["startkey"] = [fkey, arg_val.lower()]
                        view_params["endkey"] = [fkey, arg_val.lower() + u"\u9999"]
                elif lookup_field.modifier == "gte":
                    lookup_field.modifier = None
                    if fkey == None:
                        view_params["startkey"] = arg_val
                    else:
                        view_params["startkey"] = [fkey, arg_val]
                elif lookup_field.modifier == "lte":
                    lookup_field.modifier = None
                    if fkey == None:
                        view_params["endkey"] = arg_val
                    else:
                        view_params["endkey"] = [fkey, arg_val]
                elif lookup_field.modifier == "gt":
                    if isinstance(arg_val, int):
                        arg_val+=1
                    else:
                        raise ValueError, "Query modification '%s' not supported for values of '%s'" % (lookup_field.modifier, type(arg_val))
                    lookup_field.modifier = None
                    if fkey == None:
                        view_params["startkey"] = arg_val
                    else:
                        view_params["startkey"] = [fkey, arg_val]
                else:
                    raise ValueError, "Query modification '%s' not supported" % lookup_field.modifier
            else:
                if fkey == None:
                    view_params["key"] = arg_val
                else:
                    view_params["key"] = [fkey, arg_val]
            
            fcount += 1
        
        if fcount == 0:
            return None, view_params
        
        return lookup_field, view_params

class DocumentManagerDescriptor(object):
    def __init__(self, manager):
        self.manager = manager

    def __get__(self, instance, type=None):
        if instance != None:
            raise ValueError, "DocumentManager isn't accessible via %s instances" % type.__name__
        return self.manager
