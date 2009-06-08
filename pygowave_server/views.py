
#
# PyGoWave Server - The Python Google Wave Server
# Copyright 2009 Patrick Schneider <patrick.p2k.schneider@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from django.shortcuts import render_to_response
from django.template import RequestContext
from django.http import HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.core.exceptions import ObjectDoesNotExist
from django.contrib import auth
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.conf import settings as django_settings

from pygowave_server.forms import ParticipantProfileForm, GadgetRegistryForm
from pygowave_server.models import Participant, Gadget

def index(request):
	auth_fail = False
	
	# Handle logout
	if request.user.is_authenticated() and request.GET.has_key("logout"):
		auth.logout(request)
	
	if request.user.is_authenticated(): # Kick auth'd users to home view
		return HttpResponseRedirect(reverse('pygowave_server.views.home'))
	
	# Handle login
	if request.method == "POST":
		login_form = auth.forms.AuthenticationForm(data=request.POST)
		if login_form.is_valid():
			auth.login(request, login_form.get_user())
			return HttpResponseRedirect(reverse('pygowave_server.views.home'))
		else:
			auth_fail = True
	else:
		login_form = auth.forms.AuthenticationForm()
	
	online_count = Participant.objects.online_count()
	users_count = User.objects.count()
		
	return render_to_response('pygowave_server/index.html', {"login_form": login_form, "auth_fail": auth_fail, "online_count": online_count, "users_count": users_count}, context_instance=RequestContext(request))

@login_required
def home(request):
	online_count = Participant.objects.online_count()
	users_count = User.objects.count()
	return render_to_response('pygowave_server/home.html', {"username": request.user.username, "online_count": online_count, "users_count": users_count}, context_instance=RequestContext(request))

@login_required
def settings(request):
	
	try:
		profile_obj = request.user.get_profile()
	except ObjectDoesNotExist:
		profile_obj = None
	
	if request.method == "POST":
		profile_form = ParticipantProfileForm(data=request.POST, instance=profile_obj)
		if profile_form.is_valid():
			if profile_obj == None:
				profile_obj = form.save(commit=False)
				profile_obj.id = "%s@%s" % (request.user.username, settings.WAVE_DOMAIN)
				profile_obj.name = request.user.username
				profile_obj.user = request.user
				profile_obj.save()
			else:
				profile_form.save()
	else:
		profile_form = ParticipantProfileForm(instance=profile_obj)
	
	return render_to_response('pygowave_server/settings.html', {"profile_form": profile_form}, context_instance=RequestContext(request))

@login_required
def wave_list(request):
	
	return render_to_response('pygowave_server/wave_list.html', {}, context_instance=RequestContext(request))

@login_required
def my_gadgets(request):
	
	if request.GET.has_key("delete"):
		try:
			request.user.my_gadgets.get(pk=request.GET["delete"]).delete()
		except:
			pass # Silent error on hacking attempt
	
	gadget_registered = False
	if request.method == "POST":
		form = GadgetRegistryForm(data=request.POST, files=request.FILES)
		
		if form.is_valid():
			gadget = form.save(commit=False)
			gadget.by_user = request.user
			if not form.external_url():
				gadget.hosted_filename = form.cleaned_data["upload"]
			gadget.save()
			
			# New form
			gadget_registered = True
			form = GadgetRegistryForm()
	else:
		form = GadgetRegistryForm()
	
	my_gadgets = request.user.my_gadgets.all()
	return render_to_response('pygowave_server/my_gadgets.html',
							  {"my_gadgets": my_gadgets,
							   "form": form,
							   "gadget_registered": gadget_registered},
							  context_instance=RequestContext(request))

@login_required
def all_gadgets(request):
	
	gadgets = Gadget.objects.all()
	return render_to_response('pygowave_server/all_gadgets.html', {"gadgets": gadgets}, context_instance=RequestContext(request))
