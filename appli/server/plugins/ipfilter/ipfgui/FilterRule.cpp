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
// FilterRule.cpp: implementation of the CFilterRule class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "ipfgui.h"
#include "FilterRule.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CFilterRule::CFilterRule()
{
	m_strIP.Empty();
	m_strNetMask.Empty();
	m_nAction=FFSS_FILTER_ACTION_ACCEPT;
	m_strName.Empty();
	m_bGlobal=false;
}

CFilterRule::~CFilterRule()
{

}
