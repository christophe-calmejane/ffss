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
// FilterRule.h: interface for the CFilterRule class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FILTERRULE_H__D41F720D_9F25_440D_818F_03E2B56BA452__INCLUDED_)
#define AFX_FILTERRULE_H__D41F720D_9F25_440D_818F_03E2B56BA452__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CFilterRule
{
public:
	CString	m_strIP;
	CString	m_strNetMask;
	int		m_nAction;
	CString	m_strName;

	// This bool is set if the rule must be applied to all chains
	bool	m_bGlobal;

	CFilterRule();
	virtual ~CFilterRule();

};

#endif // !defined(AFX_FILTERRULE_H__D41F720D_9F25_440D_818F_03E2B56BA452__INCLUDED_)
