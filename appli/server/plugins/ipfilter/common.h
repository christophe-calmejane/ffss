#ifndef __COMMON_H__
#define __COMMON_H__

typedef unsigned int FFSS_FILTER_ACTION;

FFSS_FILTER_ACTION GetActionFromString(const char* szAction);
const char*	GetStringFromAction( FFSS_FILTER_ACTION nAction );

#endif
