#coding=utf-8
from django import forms
from django.contrib.auth.models import User
from django.utils.translation import ugettext_lazy as _

class LoginForm(forms.Form):
    username=forms.CharField(max_length=30,widget=forms.TextInput(attrs={'size': 25,}))
    password=forms.CharField(max_length=30,widget=forms.PasswordInput(attrs={'size': 25,}))