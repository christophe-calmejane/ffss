/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
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

