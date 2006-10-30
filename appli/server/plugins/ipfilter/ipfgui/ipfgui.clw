; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
ClassCount=5
Class1=CIpfguiApp
LastClass=CChainPageDlg
NewFileInclude2=#include "ipfgui.h"
ResourceCount=3
NewFileInclude1=#include "stdafx.h"
Resource1=IDD_CHAIN_PAGE (English (U.S.))
Resource2=IDD_ADDRULE_DLG
Class2=CAddRuleDlg
LastTemplate=CPropertySheet
Class3=CChainPageDlg
Class4=CConfirmDlg
Class5=CChainsSheet
Resource3=IDD_CONFIRM

[CLS:CIpfguiApp]
Type=0
HeaderFile=ipfgui.h
ImplementationFile=ipfgui.cpp
Filter=N
LastObject=CIpfguiApp

[DLG:IDD_ADDRULE_DLG]
Type=1
Class=CAddRuleDlg
ControlCount=13
Control1=IDC_RULE_NAME,edit,1350631552
Control2=IDC_RULE_IP,SysIPAddress32,1342242816
Control3=IDC_RULE_MASK,SysIPAddress32,1342242816
Control4=IDC_RULE_ACTION,combobox,1344340227
Control5=IDC_CHAIN_THIS,button,1342177289
Control6=IDC_CHAIN_ALL,button,1342177289
Control7=IDOK,button,1342242817
Control8=IDCANCEL,button,1342242816
Control9=IDC_LRULENAME,static,1342308352
Control10=IDC_LIP,static,1342308352
Control11=IDC_LNETMASK,static,1342308352
Control12=IDC_LACTION,static,1342308352
Control13=IDC_LAPPLY,static,1342308352

[DLG:IDD_CHAIN_PAGE (English (U.S.))]
Type=1
Class=CChainPageDlg
ControlCount=6
Control1=IDC_RULES,SysListView32,1350631433
Control2=IDC_POLICY,combobox,1344340227
Control3=IDC_MOVEUP,button,1342242816
Control4=IDC_MOVEDOWN,button,1342242816
Control5=IDC_LRULES,static,1342308352
Control6=IDC_LDEFAULTRULE,button,1342177287

[DLG:IDD_CONFIRM]
Type=1
Class=CConfirmDlg
ControlCount=6
Control1=IDOK,button,1342242817
Control2=IDNO,button,1342242816
Control3=IDCANCEL,button,1342242816
Control4=IDC_LDELRULE,static,1342308352
Control5=IDC_RULEMSG,static,1342308352
Control6=IDC_LFROM,static,1342308352

[CLS:CAddRuleDlg]
Type=0
HeaderFile=AddRuleDlg.h
ImplementationFile=AddRuleDlg.cpp
BaseClass=CDialog
Filter=D
LastObject=CAddRuleDlg

[CLS:CChainPageDlg]
Type=0
HeaderFile=ChainPageDlg.h
ImplementationFile=ChainPageDlg.cpp
BaseClass=CPropertyPage
Filter=D
LastObject=IDC_POLICY
VirtualFilter=dWC

[CLS:CConfirmDlg]
Type=0
HeaderFile=ConfirmDlg.h
ImplementationFile=ConfirmDlg.cpp
BaseClass=CDialog
Filter=D
LastObject=CConfirmDlg
VirtualFilter=dWC

[CLS:CChainsSheet]
Type=0
HeaderFile=ChainsSheet.h
ImplementationFile=ChainsSheet.cpp
BaseClass=CPropertySheet
Filter=W
VirtualFilter=hWC

