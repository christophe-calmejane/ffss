#include "common.h"
#include "string.h"

static char*	szActionString[] = { "","Accept", "Reject" };

/////////////////////////////////////////////////////////////////////////////
FFSS_FILTER_ACTION GetActionFromString(const char* szAction)
{
	int i;

	for( i=0; i< (sizeof(szActionString)/sizeof(char*)); i++ ) {
		if( strcmp(szAction,szActionString[i])==0 ) {
			return(i);
		}
	}
	return(-1);
}

/////////////////////////////////////////////////////////////////////////////
const char*	GetStringFromAction( FFSS_FILTER_ACTION nAction )
{
	return(szActionString[nAction]);
}

