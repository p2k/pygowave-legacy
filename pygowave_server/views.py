
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

from pygowave_server.forms import ParticipantProfileForm, GadgetRegistryForm, NewWaveForm
from pygowave_server.models import Participant, Gadget, Wave, GadgetElement
from pygowave_server.engine import GadgetLoader

from datetime import datetime, timedelta
import urllib2, os.path
from lxml.etree import XMLSyntaxError

from pycow import translate_file

def index(request):
	auth_fail = False
	
	# Handle logout
	if request.user.is_authenticated() and request.GET.has_key("logout"):
		try:
			p = request.user.get_profile()
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
	return render_to_response('pygowave_server/home.html', {"username": request.user.username, "online_count": online_count, "users_count": users_count}, context_instance=RequestContext(request))

@login_required
def settings(request):
	
	try:
		profile_obj = request.user.get_profile()
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
	
	participant = request.user.get_profile()
	
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
	return render_to_response('pygowave_server/wave_list.html', {"waves": waves, "form": form}, context_instance=RequestContext(request))

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

@login_required
def wave(request, wave_id):
	
	try:
		wave = Wave.objects.get(pk=wave_id)
	except ObjectDoesNotExist:
		if request.is_ajax():
			return HttpResponse("Error: Invalid Wave ID")
		else:
			return render_to_response('pygowave_server/invalid_wave.html', context_instance=RequestContext(request))
	
	participant = request.user.get_profile()

	wavelet = wave.root_wavelet()
	
	if wavelet.participants.filter(id=participant.id).count() == 0:
		if request.is_ajax():
			return HttpResponse("Error: No permission")
		else:
			return render_to_response('pygowave_server/no_permission.html', context_instance=RequestContext(request))
	
	if request.is_ajax():
		if request.GET.has_key("get_participant"):
			try:
				p_obj = wavelet.participants.get(pk=request.GET["get_participant"])
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
			return render_to_response('pygowave_server/participant_info.html', {"participant": p}, context_instance=RequestContext(request))
		
		return HttpResponse("Error: Unknown request")
	else:
		conn = participant.create_new_connection()
		wave_access_key = {
			"rx": conn.rx_key,
			"tx": conn.tx_key,
		}
		
		gadgets = Gadget.objects.all()
		return render_to_response('pygowave_server/on_the_wave.html', {
			"gadgets": gadgets,
			"wave_access_key": wave_access_key,
			"wavelet_title": wavelet.title,
			"wave_id": wave_id,
			"wavelet_id": wavelet.id,
			"participant_id": participant.id
		}, context_instance=RequestContext(request))

@login_required
def search_participants(request):
	
	if request.GET.has_key("q"):
		q = request.GET["q"]
	else:
		q = ""
	
	if len(q) < 3: # TODO - Hardcoded
		return render_to_response('pygowave_server/search_participants.html', {"too_short": 3}, context_instance=RequestContext(request))
	
	me = request.user.get_profile()

	lst = []
	for p in Participant.objects.filter(name__icontains=q):
		if p == me:
			continue
		if p.avatar:
			avatar = p.avatar
		else:
			avatar = django_settings.AVATAR_URL + "default.png"
		lst.append({"id": p.id, "avatar": avatar, "profile": p.profile, "name": p.name})
	
	return render_to_response('pygowave_server/search_participants.html', {"matches": lst}, context_instance=RequestContext(request))

def gadget_loader(request):
	"""
	Load a gadget from any URL.
	
	"""
	
	if not request.GET.has_key("url"):
		return render_to_response('pygowave_server/gadget_error.html', {"error_message": _(u"No URL specified.")})
	
	url = request.GET["url"]

	# Directly load hosted gadgets
	try:
		gadget_obj = Gadget.objects.get(url=url)
		if gadget_obj.is_hosted():
			url = "file://%s%s" % (django_settings.GADGET_ROOT, gadget_obj.hosted_filename)
	except ObjectDoesNotExist:
		pass
	
	try:
		gadget = GadgetLoader(url)
	except urllib2.HTTPError:
		return render_to_response('pygowave_server/gadget_error.html', {"error_message": _(u"Gadget could not be downloaded.")}, context_instance=RequestContext(request))
	except XMLSyntaxError:
		return render_to_response('pygowave_server/gadget_error.html', {"error_message": _(u"Gadget quick-check failed: Bad XML format.")}, context_instance=RequestContext(request))
	except ValueError, e:
		return render_to_response('pygowave_server/gadget_error.html', {"error_message": _(u'Gadget quick-check failed: %s.') % (e.args[0])}, context_instance=RequestContext(request))
	
	if request.GET.has_key("gadget_id"):
		gadget_id = request.GET["gadget_id"]
		try:
			ge = GadgetElement.objects.get(pk=gadget_id)
			gadget.update_prefs(ge.get_userprefs())
		except:
			return render_to_response('pygowave_server/gadget_error.html', {"error_message": _(u"GadgetElement could not be found.")}, context_instance=RequestContext(request))
	else:
		gadget_id = None

	return render_to_response('pygowave_server/gadget_wrapper.html', {"gadget": gadget, "url_parameters": simplejson.dumps(request.GET), "gadget_id": gadget_id}, context_instance=RequestContext(request))

COMMON_FOLDER = os.path.dirname(os.path.abspath(__file__)) + os.path.sep + "common" + os.path.sep
def pycow(request, filename):
	"""
	Translate a Python file on-the-fly or use a cached version if possible.
	
	"""
	
	module = "pygowave." + filename[:-3]
	cachefile = django_settings.PYCOW_CACHE_ROOT + filename
	srcfile = COMMON_FOLDER + filename[:-2] + "py"
	
	if not os.path.exists(cachefile) or  os.path.getmtime(srcfile) > os.path.getmtime(cachefile):
		translate_file(srcfile, cachefile, namespace=module, warnings=False)
	
	return HttpResponse(open(cachefile, "r").read(), mimetype="text/javascript")
