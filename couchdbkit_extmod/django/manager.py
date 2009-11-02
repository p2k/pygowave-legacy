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

from django.core.exceptions import ObjectDoesNotExist

from loading import SIMPLE_VIEW_TEMPLATE

__all__ = ["DocumentManager"]

class DocumentManager(object):
    filters = []

    def __init__(self, filters=[]):
        self.filters = self.filters + filters

    @classmethod
    def _copy_to_class(cls, doc, name):
        thecopy = cls()
        thecopy.contribute_to_class(doc, name)
    
    def contribute_to_class(self, doc, name):
        self.doc = doc
        document_module = sys.modules[doc.__module__]
        self.app_label = document_module.__name__.split('.')[-2]
        setattr(doc, name, DocumentManagerDescriptor(self))
    
    def create(self, **kwargs):
        return self.doc(**kwargs)
    
    def get(self, id):
        return self.doc.get(id)
    
    def view(self, view_name, wrapper=None, dynamic_properties=True, **params):
        return self.doc.view(view_name, wrapper=wrapper, dynamic_properties=dynamic_properties, **params)
    
    def temp_view(self, design, wrapper=None, dynamic_properties=True, **params):
        return self.doc.temp_view(design, wrapper=wrapper, dynamic_properties=dynamic_properties, **params)
    
    def _generate_views(self):
        views = {}
        for filter in self.filters:
            view_name = "%s_by_%s" % (self.doc.__name__, filter)
            views[view_name] = {"map": SIMPLE_VIEW_TEMPLATE % (self.doc.__name__, filter)}
        return views
    
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
                    view_params["key"] = arg_val
                elif spl[1] == "gte":
                    view_params["startkey"] = arg_val
                elif spl[1] == "lte":
                    view_params["endkey"] = arg_val
                else:
                    raise ValueError, "Query modification '%s' not supported" % spl[1]
            else:
                view_params["key"] = arg_val
            
            fcount += 1
        
        view_name = "%s/%s_by_%s" % (self.app_label, self.doc.__name__, lookup_field)
        
        if not lookup_field in self.filters:
            print "Warning: Temp View used for lookup '%s'" % (view_name)
            return self.temp_view({"map": SIMPLE_VIEW_TEMPLATE % (self.doc.__name__, lookup_field)}, **view_params)
        else:
            return self.view(view_name, **view_params)

class DocumentManagerDescriptor(object):
    def __init__(self, manager):
        self.manager = manager

    def __get__(self, instance, type=None):
        if instance != None:
            raise ValueError, "DocumentManager isn't accessible via %s instances" % type.__name__
        return self.manager
