from django.shortcuts import render
from django.shortcuts import render_to_response
from django.http import HttpResponse, HttpResponseRedirect
from django import forms
from distcc_frontend.models import User
from sendFile import sendFile

# Authentication 
from django.contrib.auth.models import User
from django.contrib.auth import authenticate, login as auth_login ,logout as auth_logout
from django.shortcuts import render_to_response
from django.template import RequestContext

# form
from forms import LoginForm
from django.contrib import messages
from django.utils.translation import ugettext_lazy as _
from django.core.urlresolvers import reverse

class UserForm(forms.Form):
    username = forms.CharField()
    uploadfile = forms.FileField()

def home_page(request):
    #return HttpResponse("hello")
    is_authenticated = False
    if request.user.is_authenticated():
        is_authenticated = True
    context = {"is_authenticated":is_authenticated}

    if request.method == 'GET':
        return render(request, 'index.html', context)

# Create your views here.
def login(request):
    '''Login View'''
    template_var={}
    form = LoginForm()    
    if request.method == 'POST':
        form=LoginForm(request.POST.copy())
        if form.is_valid():
            if _login(request,form.cleaned_data["username"],form.cleaned_data["password"]):
                return HttpResponseRedirect(reverse("index"))
            return HttpResponseRedirect(reverse("login"))
    template_var["form"]=form        
    return render_to_response("signin.html",template_var,context_instance=RequestContext(request))
    
def _login(request,username,password):
    '''Login Key Functions'''
    ret=False
    user=authenticate(username=username,password=password)
    if user:
        if user.is_active:
            auth_login(request,user)
            ret=True
        else:
            messages.add_message(request, messages.INFO, _(u'User not active'))
    else:
        messages.add_message(request, messages.INFO, _(u'Username or password wrong'))
    return ret

def logout(request):
    auth_logout(request)
    return HttpResponseRedirect(reverse("index"))

def signin_page(request):
    context = {}

    if request.method == 'GET':
        return render(request, 'signin.html', context)

def progress_page(request):
    if not request.user.is_authenticated():
        return HttpResponseRedirect(reverse("index"))
    context = {}

    if request.method == 'GET':
        return render(request, 'progress.html', context)


def upload_page(request):
    if not request.user.is_authenticated():
        return HttpResponseRedirect(reverse("index"))
    context = {}
    if request.method == 'GET':
        return render(request, 'upload.html', context)
    if request.method == 'POST':
        uf = UserForm(request.POST, request.FILES)
        if uf.is_valid():
            username = uf.cleaned_data['username']
            uploadfile = uf.cleaned_data['uploadfile']
            print uploadfile
            user = User()
            user.username = username
            user.uploadFile = uploadfile
            user.save()
            sendFile("upload/" + str(uploadfile))
            #fp = file('/uploadFile/' + uf.cleaned_data['uploadfile'].name, 'wb')
            #s = uf.cleaned_data['uploadfile'].read()
            #fp.write(s)
            #fp.close()
            #print HttpResponse('File Uploaded')

        return render_to_response('upload.html', {'uf':uf})


