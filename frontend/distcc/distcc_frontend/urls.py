from django.conf.urls import patterns, include, url

urlpatterns = patterns('',
    #url(r'^$', 'distcc_frontend.views.home_page'),
    url(r'^home/$', 'distcc_frontend.views.home_page', name='index'),
    url(r'^logout/$', 'distcc_frontend.views.logout', name='logout'),
    url(r'^signin/$', 'distcc_frontend.views.login', name = 'login' ),
    url(r'^progress/$', 'distcc_frontend.views.progress_page', name = 'progress' ),
    url(r'^upload/$', 'distcc_frontend.views.upload_page', name = 'upload'),
)