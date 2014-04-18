from django.shortcuts import render
from django.shortcuts import render_to_response
from django.http import HttpResponse
from django import forms
from distcc_frontend.models import User
from sendFile import sendFile

class UserForm(forms.Form):
    username = forms.CharField()
    uploadfile = forms.FileField()

def home_page(request):
    #return HttpResponse("hello")
    context = {}

    if request.method == 'GET':
        return render(request, 'index.html', context)

# Create your views here.

def signin_page(request):
    context = {}

    if request.method == 'GET':
        return render(request, 'signin.html', context)

def progress_page(request):
    context = {}

    if request.method == 'GET':
        return render(request, 'progress.html', context)


def upload_page(request):
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


