
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
from django.http import HttpResponseRedirect, HttpResponse
from django.core.urlresolvers import reverse
from django.core.exceptions import ObjectDoesNotExist
from django.contrib import auth
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.conf import settings as django_settings
from django.utils.translation import ugettext as _
from django.utils import simplejson

from pygowave_server.forms import ParticipantProfileForm, GadgetRegistryForm, DuplicateGadgetForm, NewWaveForm
from pygowave_server.models import Participant, Gadget, Wave, GadgetElement

from datetime import datetime, timedelta
import urllib2

def index(request):
	auth_fail = False
	
	# Handle logout
	if request.user.is_authenticated() and request.GET.has_key("logout"):
		try:
			p = Participant.objects.get(user__id=request.user.id)
			p.last_contact = datetime.now() + timedelta(minutes=django_settings.ONLINE_TIMEOUT_MINUTES+1)
			p.save()
		except:
			pass
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
	users_count = User.objects.filter(is_active=True).count()
		
	return render_to_response('pygowave_server/index.html', {"login_form": login_form, "auth_fail": auth_fail, "online_count": online_count, "users_count": users_count}, context_instance=RequestContext(request))

@login_required
def home(request):
	online_count = Participant.objects.online_count()
	users_count = User.objects.filter(is_active=True).count()
	return render_to_response('pygowave_server/home.html', {"online_count": online_count, "users_count": users_count}, context_instance=RequestContext(request))

@login_required
def settings(request):
	
	try:
		profile_obj = Participant.objects.get(user__id=request.user.id)
	except ObjectDoesNotExist:
		profile_obj = None
	
	if request.method == "POST":
		profile_form = ParticipantProfileForm(data=request.POST, files=request.FILES, instance=profile_obj)
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
	
	participant = Participant.objects.get(user__id=request.user.id)
	
	if request.method == "POST":
		form = NewWaveForm(data=request.POST)
		if form.is_valid():
			wave = Wave.objects.create_and_init_new_wave(participant, form.cleaned_data["title"])
			return HttpResponseRedirect(reverse("pygowave_server.views.wave", kwargs={"wave_id": wave.id}))
	else:
		form = NewWaveForm()
	
	waves = []
	for wavelet in participant.wavelets.all():
		wave = wavelet.wave
		waves.append({
			"id": wave.id,
			"title": wavelet.title,
			"participants_count": wavelet.participants.count(),
			"created": wavelet.created,
			"creator_name": wavelet.creator.name,
		})
	return render_to_response('pygowave_server/waves/wave_list.html', {"waves": waves, "form": form}, context_instance=RequestContext(request))

@login_required
def my_gadgets(request):
	
	if request.GET.has_key("delete"):
		try:
			request.user.my_gadgets.get(pk=request.GET["delete"]).delete()
		except:
			pass # Silent error on hacking attempt
	
	duplicate = False
	can_overwrite = False
	gadget_registered = False
	if request.method == "POST":
		if not request.GET.has_key("duplicate"):
			form = GadgetRegistryForm(data=request.POST, files=request.FILES)
			if form.is_valid():
				gadget = form.save(commit=False)
				gadget.by_user = request.user
				if not form.external_url():
					gadget.hosted_filename = form.cleaned_data["upload"]
				try:
					old_gadget = Gadget.objects.get(title=gadget.title)
				except ObjectDoesNotExist:
					gadget.save()
					
					# New form
					gadget_registered = True
					form = GadgetRegistryForm()
				else:
					duplicate = True
					if old_gadget.by_user == request.user:
						can_overwrite = True
					form = DuplicateGadgetForm(initial={
						"external": form.cleaned_data["external"],
						"title": form.cleaned_data["title"],
						"description": form.cleaned_data["description"],
						"url": form.cleaned_data["url"]
					})
		else:
			form = DuplicateGadgetForm(data=request.POST)
			if request.POST.has_key("cancel"):
				form = GadgetRegistryForm()
			elif form.is_valid():
				gadget = form.save(commit=False)
				gadget.by_user = request.user
				if not form.external_url():
					gadget.hosted_filename = form.cleaned_data["url"].split("/")[-1]
				try:
					old_gadget = Gadget.objects.get(title=gadget.title)
				except ObjectDoesNotExist:
					gadget.save()
					
					gadget_registered = True
					form = GadgetRegistryForm()
				else:
					if old_gadget.by_user == request.user:
						old_gadget.url = gadget.url
						old_gadget.description = gadget.description
						old_gadget.hosted_filename = gadget.hosted_filename
						old_gadget.save()
						
						gadget_registered = True
						form = GadgetRegistryForm()
					else:
						duplicate = True
			else:
				duplicate = True
	else:
		form = GadgetRegistryForm()
	
	my_gadgets = request.user.my_gadgets.all()
	return render_to_response('pygowave_server/gadgets/my_gadgets.html',
							  {"my_gadgets": my_gadgets,
							   "form": form,
							   "gadget_registered": gadget_registered,
							   "duplicate": duplicate,
							   "can_overwrite": can_overwrite},
							  context_instance=RequestContext(request))

@login_required
def all_gadgets(request):
	
	gadgets = Gadget.objects.all()
	return render_to_response('pygowave_server/gadgets/all_gadgets.html', {"gadgets": gadgets}, context_instance=RequestContext(request))

@login_required
def wave(request, wave_id):
	
	try:
		wave = Wave.objects.get(id=wave_id)
	except ObjectDoesNotExist:
		if request.is_ajax():
			return HttpResponse("Error: Invalid Wave ID")
		else:
			return render_to_response('pygowave_server/waves/invalid_wave.html', context_instance=RequestContext(request))
	
	participant = Participant.objects.get(user__id=request.user.id)

	wavelet = wave.root_wavelet()
	
	if wavelet.participants.filter(id=participant.id).count() == 0:
		if request.is_ajax():
			return HttpResponse("Error: No permission")
		else:
			return render_to_response('pygowave_server/waves/no_permission.html', context_instance=RequestContext(request))
	
	if request.is_ajax():
		if request.GET.has_key("get_participant"):
			try:
				p_obj = wavelet.participants.get(id=request.GET["get_participant"])
			except ObjectDoesNotExist:
				return HttpResponse("Error: Participant not found")
			p = {
				"name": p_obj.name,
				"profile": p_obj.profile,
			}
			if p_obj.avatar:
				p["avatar"] = p_obj.avatar
			else:
				p["avatar"] = django_settings.AVATAR_URL + "default.png"
			return render_to_response('pygowave_server/contacts/participant_info.html', {"participant": p}, context_instance=RequestContext(request))
		
		return HttpResponse("Error: Unknown request")
	else:
		conn = participant.create_new_connection()
		wave_access_key = {
			"rx": conn.rx_key,
			"tx": conn.tx_key,
		}
		
		gadgets = Gadget.objects.all()
		return render_to_response('pygowave_server/waves/on_the_wave.html', {
			"gadgets": gadgets,
			"wave_access_key": wave_access_key,
			"wavelet_title": wavelet.title,
			"wave_id": wave_id,
			"wavelet_id": wavelet.id,
			"participant_id": participant.id
		}, context_instance=RequestContext(request))

def gadget_loader(request):
	"""
	Load a gadget from any URL.
	
	"""
	try:
		from lxml.etree import XMLSyntaxError
	except ImportError:
		return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u"This server currently does not support Gadgets (lxml package missing).")}, context_instance=RequestContext(request))
	
	from pygowave_server.engine import GadgetLoader
	
	if not request.GET.has_key("url"):
		return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u"No URL specified.")})
	
	url = request.GET["url"]

	# Directly load hosted gadgets
	gadget_obj = Gadget.objects.filter(url=url)
	if gadget_obj.count() > 0:
		gadget_obj = gadget_obj.all()[0]
		if gadget_obj.is_hosted():
			url = "file://%s%s" % (django_settings.GADGET_ROOT, gadget_obj.hosted_filename)
	
	try:
		gadget = GadgetLoader(url)
	except (urllib2.HTTPError, urllib2.URLError):
		return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u"Gadget could not be downloaded.")}, context_instance=RequestContext(request))
	except XMLSyntaxError:
		return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u"Gadget quick-check failed: Bad XML format.")}, context_instance=RequestContext(request))
	except ValueError, e:
		return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u'Gadget quick-check failed: %s.') % (e.args[0])}, context_instance=RequestContext(request))
	
	if request.GET.has_key("gadget_id"):
		gadget_id = int(request.GET["gadget_id"])
		if gadget_id > 0:
			try:
				ge = GadgetElement.objects.get(pk=gadget_id)
			except:
				return render_to_response('pygowave_server/gadgets/gadget_error.html', {"error_message": _(u"GadgetElement could not be found.")}, context_instance=RequestContext(request))
			gadget.update_prefs(ge.get_userprefs())
	else:
		gadget_id = None

	if request.GET.has_key("wrapper_script"):
		wrapper_script_url = request.GET["wrapper_script"]
	else:
		wrapper_script_url = django_settings.MEDIA_URL + "js/gadget-wrapper.js"

	return render_to_response(
		'pygowave_server/gadgets/gadget_wrapper.html',
		{
			"gadget": gadget,
			"url_parameters": simplejson.dumps(request.GET),
			"gadget_id": gadget_id,
			"wrapper_script_url": wrapper_script_url
		},
		context_instance=RequestContext(request)
	)
