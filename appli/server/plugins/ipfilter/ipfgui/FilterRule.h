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
