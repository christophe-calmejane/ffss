// LangSelect.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "skyutils.h"

#define ST_WINTITLE  0
#define ST_LTEXT     1
#define ST_OK        2
#define ST_CANCEL    3
#define ST_EN		 4
#define ST_FR		 5
#define ST_DE		 6
#define ST_ES		 7

#define LANGUAGES_COUNT		4
#define FAVORITE_LANGUAGE	"HKEY_LOCAL_MACHINE\\Software\\FFSS\\FavoriteLanguage"

char *ST[][8] = { {	"FFSS language selector", 
					"Choose your favorite language :\r\rSome applications may not use this setting...but should =)",
					"OK", "Cancel", 
					"English", "French", "German", "Spanish" },	
				{   "Sélecteur de langage FFSS", 
					"Choisissez votre langue favorite pour les applications FFSS :\r\rCertaines applications peuvent ne pas tenir compte de ce réglage.",
					"OK", "Annuler", 
					"Anglais","Français", "Allemand", "Espagnol" },

				{   "[non traduit]", 
					"[non traduit]",
					"OK", "Annuler", 
					"Anglais","Français", "Allemand", "Espagnol" },

				{   "[non traduit]", 
					"[non traduit]",
					"OK", "Annuler", 
					"Anglais","Français", "Allemand", "Espagnol" }
				};
char **CurST;

int nCurrentLanguage=0;

///////////////////////////////////////////////////////////////////////////////
unsigned int PrefixToInt(const char szPrefix[] )
{
	if( stricmp(szPrefix,"FR")==0 ) {
		return(1);
	}

	if( stricmp(szPrefix,"DE")==0 ) {
		return(2);
	}

	if( stricmp(szPrefix,"ES")==0 ) {
		return(3);
	}

	return(0);
}

///////////////////////////////////////////////////////////////////////////////
unsigned int GetFFSSLanguageFromRegistry()
{
	char buffer[16];

	SU_RB_GetStrValue(FAVORITE_LANGUAGE,buffer,sizeof(buffer),"EN");

	return( PrefixToInt(buffer) );
}

///////////////////////////////////////////////////////////////////////////////
void SetFFSSLanguageToRegistry(unsigned int nLanguage)
{
	switch( nLanguage ) {
		case 1 :
			SU_RB_SetStrValue(FAVORITE_LANGUAGE,"FR");
			break;
		case 2 :
			SU_RB_SetStrValue(FAVORITE_LANGUAGE,"DE");
			break;
		case 3 :
			SU_RB_SetStrValue(FAVORITE_LANGUAGE,"ES");
			break;
		default:
			SU_RB_SetStrValue(FAVORITE_LANGUAGE,"EN");
	}
}

///////////////////////////////////////////////////////////////////////////////
void OnInitDialog(HWND hDlg)
{
	HWND	hLB;
	HWND	hLabel;
	HWND	hBtnOK;
	HWND	hBtnCancel;
	HWND	hwndOwner; 
	RECT	rc, rcDlg, rcOwner; 

	hLabel = GetDlgItem(hDlg,(int)MAKEINTRESOURCE(IDC_LTEXT));
	hLB = GetDlgItem(hDlg,(int)MAKEINTRESOURCE(IDC_COMBO));
	hBtnOK = GetDlgItem(hDlg,(int)MAKEINTRESOURCE(IDOK));
	hBtnCancel = GetDlgItem(hDlg,(int)MAKEINTRESOURCE(IDCANCEL));

	SetWindowText(hDlg,CurST[ST_WINTITLE]);
	SetWindowText(hLabel,CurST[ST_LTEXT]);
	SetWindowText(hBtnOK,CurST[ST_OK]);
	SetWindowText(hBtnCancel,CurST[ST_CANCEL]);
	SendMessage( hLB, CB_ADDSTRING,0, (long)CurST[ST_EN] );
	SendMessage( hLB, CB_ADDSTRING,0, (long)CurST[ST_FR] );
	SendMessage( hLB, CB_ADDSTRING,0, (long)CurST[ST_DE] );
	SendMessage( hLB, CB_ADDSTRING,0, (long)CurST[ST_ES] );

	SendMessage( hLB, CB_SETCURSEL, nCurrentLanguage, 0 );

	// Get the owner window and dialog box rectangles. 

	if ((hwndOwner = GetParent(hDlg)) == NULL) 
	{
		hwndOwner = GetDesktopWindow(); 
	}

	GetWindowRect(hwndOwner, &rcOwner); 
	GetWindowRect(hDlg, &rcDlg); 
	CopyRect(&rc, &rcOwner); 

	 // Offset the owner and dialog box rectangles so that 
	 // right and bottom values represent the width and 
	 // height, and then offset the owner again to discard 
	 // space taken up by the dialog box. 

	OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
	OffsetRect(&rc, -rc.left, -rc.top); 
	OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 

	 // The new position is the sum of half the remaining 
	 // space and the owner's original position. 

	SetWindowPos(hDlg, 
		HWND_TOP, 
		rcOwner.left + (rc.right / 2), 
		rcOwner.top + (rc.bottom / 2), 
		0, 0,          // ignores size arguments 
		SWP_NOSIZE); 
}

///////////////////////////////////////////////////////////////////////////////
void OnOK(HWND hDlg)
{
	HWND	hLB;
	unsigned int nSelIndex;

	hLB = GetDlgItem(hDlg,(int)MAKEINTRESOURCE(IDC_COMBO));

	nSelIndex=SendMessage( hLB, CB_GETCURSEL,0,0);
	if( nSelIndex!=CB_ERR ) {
		SetFFSSLanguageToRegistry(nSelIndex);
	}
}

///////////////////////////////////////////////////////////////////////////////
BOOL CALLBACK wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message) {
		case WM_COMMAND:
			switch(LOWORD(wParam))
			{
				case IDOK:
					OnOK(hwnd);
					EndDialog(hwnd,1);
					PostQuitMessage(0);
				return TRUE;
				
				case IDCANCEL:
					EndDialog(hwnd,0);
					PostQuitMessage(0);
				return TRUE;
			}
			break;

	case WM_INITDIALOG:
		OnInitDialog(hwnd);
		return TRUE;

	case WM_CREATE:
		return TRUE;

	case WM_DESTROY:
		PostQuitMessage(0);
		return TRUE;
	}
	return FALSE;
}

/*****************************************************************************/
int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
	MSG		msg;
	HWND	hDlg;
	BOOL bRet;

	nCurrentLanguage=GetFFSSLanguageFromRegistry();
	if( __argc==2 ) {
		nCurrentLanguage=PrefixToInt(__argv[1]);
	}
	CurST=ST[nCurrentLanguage];

	hDlg = CreateDialog(hInstance,MAKEINTRESOURCE(IDD_DIALOG1),NULL,wndProc);
	if(hDlg == NULL)
		return(-1);

	ShowWindow(hDlg,SW_SHOW);

	while ( (bRet = GetMessage(&msg, NULL, 0, 0)) != 0 ) 
	{ 
	    if (bRet == -1 )
	    {
		    // handle the error and possibly exit
			return(-1);
		}
		else if (!IsWindow(hDlg) || !IsDialogMessage(hDlg, &msg)) 
		{ 
	        TranslateMessage(&msg); 
			DispatchMessage(&msg); 
		} 
	} 

	DestroyWindow(hDlg);

	return 0;
}



