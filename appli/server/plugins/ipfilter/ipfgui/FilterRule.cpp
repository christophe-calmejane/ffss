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
