
:- initialization((
	logtalk_load([library(dates_loader), library(events_loader), library(metapredicates_loader), library(types_loader)]),
	logtalk_load([timer, message_counter, stop_watch])
)).
