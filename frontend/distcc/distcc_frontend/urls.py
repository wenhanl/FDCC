from django.conf.urls import patterns, include, url

urlpatterns = patterns('',
    #url(r'^$', 'distcc_frontend.views.home_page'),
    url(r'^home/$', 'distcc_frontend.views.home_page'),
    url(r'^signin/$', 'distcc_frontend.views.signin_page', name = 'signin' ),
    url(r'^progress/$', 'distcc_frontend.views.progress_page', name = 'progress' ),
    url(r'^upload/$', 'distcc_frontend.views.upload_page', name = 'upload'),
)