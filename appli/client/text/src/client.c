/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	main file
 */

#include "client.h"
#include "args.h"
#include "display.h"
#include "command.h"
#include "common.h"
#include "datastruct.h"
#include "callbacks.h"
#include "config.h"
#include "skin.h"
#include "cgi_args.h"

#include <readline/readline.h>
/* libCGI
#include <cgi.h>
*/

	/* GLOBALS */
char FCA_pwd[FFSS_MAX_PATH_LENGTH];
char FCA_conSh[FFSS_MAX_PATH_LENGTH];   /* the connected share */
char FCA_conIP[FFSS_IP_FIELD_SIZE];	/* the connected server */
SU_PClientSocket FCA_shrSkt;
char FCA_tolist_dir[FFSS_MAX_PATH_LENGTH];
char FCA_home[FFSS_MAX_DOMAIN_LENGTH];
FCA_Pskin FCA_skin;
bool FCA_exiting;

char FCA_env[][FCA_VAR_MAX]={
	"on",
	"0",
	"on",
	"on",
	"",
	"off",
	"default",
	"5",
	"5",
	FCA_SKIN_ENV_VALUES
};


void FCA_init()
{
		/* init FFSS callback structure */
	memset(&FFSS_CB,0,sizeof(FFSS_CB));
	FFSS_CB.CCB.OnNewState 			= FCA_OnNewState;
	FFSS_CB.CCB.OnBeginTCPThread		= FCA_OnBeginTCPThread;
    
	FFSS_CB.CCB.OnServerListingAnswer 	= FCA_OnServerListingAnswer;
	FFSS_CB.CCB.OnEndServerListingAnswer	= FCA_OnEndServerListingAnswer;
	FFSS_CB.CCB.OnDirectoryListingAnswer	= FCA_OnDirectoryListingAnswer;
	FFSS_CB.CCB.OnSharesListing 		= FCA_OnSharesListing;
	FFSS_CB.CCB.OnDomainListingAnswer	= FCA_OnDomainListingAnswer;
	FFSS_CB.CCB.OnSearchAnswer		= FCA_OnSearchAnswer;
    
	FFSS_CB.CCB.OnTransferActive 		= FCA_OnTransfertActive;
	FFSS_CB.CCB.OnTransferFailed		= FCA_OnTransfertFailed;
	FFSS_CB.CCB.OnTransferSuccess		= FCA_OnTransfertSuccess;
	FFSS_CB.CCB.OnInitXFer			= FCA_OnInitXFer;
	FFSS_CB.CCB.OnData			= FCA_OnData;
   
	FFSS_CB.CCB.OnMasterError		= FCA_OnMasterError;
	FFSS_CB.CCB.OnError 			= FCA_OnError;
	FFSS_CB.CCB.OnEndTCPThread 		= FCA_OnEndTCPThread;
	FFSS_CB.CCB.OnUDPError 			= FCA_OnUDPError;
	FFSS_CB.CCB.OnFatalError 		= FCA_OnFatalError;
	FFSS_CB.CCB.OnIdleTimeout 		= FCA_OnIdleTimeout;

		/* init global variables */
	FCA_args.cfg_file=FCA_CONFIG_FILES[0];
	SU_strcpy(FCA_pwd, "/$", sizeof(FCA_pwd));
	    	/* to use same functions, pwd is like //... */
/*
	FCA_ignore_usr1();
*/
	FCA_err_stream=stderr;
	FCA_exiting=false;
	FCA_conSh[0]='\0';
	FCA_conIP[0]='\0';
	FCA_shrSkt=NULL;
	FCA_sem_init();
	FCA_command=NULL;
	FCA_quiet=false;
	FCA_Servers=NULL;
	FCA_list=NULL;
	FCA_inDispServs=false;
	FCA_everListDom=false;
	FCA_posted=false;
	FCA_canChange_pwd=true;
	sprintf(FCA_prompt,"off");
	FCA_Ptrans=NULL;
	FCA_reading_file=false;
	FCA_in_comment=false;
	FCA_skin=(FCA_Pskin)&(FCA_SKINS[0]);
	sprintf(FCA_skin_name, FCA_SKINS[0].name);
#ifndef _WIN32
	sprintf(FCA_can_ansi, "on");
#else
	sprintf(FCA_can_ansi, "off");
#endif
		/* init libs */
	rl_readline_name = FCA_NAME;
	    	/* for readline completion */
	rl_attempted_completion_function = (CPPFunction *)FCA_completion;
	rl_basic_word_break_characters=" ";
    		/* debug default level */
#ifdef DEBUG
	N_DebugLevel=6;
	sprintf(FCA_debuglevel, "6");
#else
	N_DebugLevel=0;
	sprintf(FCA_debuglevel, "0");
#endif
#ifdef CGI
		/* see if we're in CGI mode */
	FCA_CGI_mode=FCA_isInCGImode();
	if(FCA_CGI_mode) {
			/* by default, in CGI mode, it's better to ignore all debug messages */
		N_DebugLevel=0;
			/* we use N_SyslogOn to delete some syslog messages in CGI mode */
		if( FFSS_GetFFSSOptions() & FFSS_OPTIONS_DEBUG)
			N_SyslogOn=0;
		sprintf(FCA_debuglevel, "0");
	}
#endif
		/* load all skins */
	FCA_load_skins();
}

void FCA_process_args()
{
	char buf[6+FFSS_MAX_PATH_LENGTH];

	if( !FCA_args.can_ansi )
		sprintf(FCA_can_ansi, "off");
	if( FCA_args.dbg_level!=NULL ) {	/* debug level */
		if(FCA_args.dbg_level[0]-'0'<0 || FCA_args.dbg_level[0]-'0'>6)
			FCA_print_warning("debug option: level must be between 0 and 6");
		else {
			N_DebugLevel=FCA_args.dbg_level[0]-'0';
			sprintf(FCA_debuglevel, "%d", N_DebugLevel);
		}
	}
	if( FCA_args.master!=NULL )
		sprintf(FCA_master, FCA_args.master);
	if( FCA_args.skin!=NULL ) {
		sprintf(FCA_skin_name, FCA_args.skin);
		FCA_upd_skin();
	}
	if( FCA_args.machToSh!=NULL )		/* get shares */
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "ls /$/%s", FCA_args.machToSh);
	else if( FCA_args.dirToLs!=NULL )	/* list a dir */
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "ls %s", FCA_args.dirToLs);
	else if( FCA_args.cmd!=NULL )	 	/* exec a command */
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "%s", FCA_args.cmd);
	else
		return;
	FCA_command=buf;
	FCA_run_once(false);
}

#ifdef CGI
void FCA_process_cgi_args()
{
	const FCA_Tskin *p;
	char buf[6+FFSS_MAX_PATH_LENGTH];
	bool dw=false;
	char *pd;
	
	if(!FCA_CGI_mode)
		return;
		/* first we must have a good skin */
	p=FCA_SKINS;
	while(p->name && !p->canCGI)
		p++;
		/* use the default skin to display this critical message */
	if(!p->name)
		FCA_crash("cannot find a skin adapted to the CGI mode");
	FCA_skin=(FCA_Pskin)p;
	sprintf(FCA_skin_name, p->name);
		/* let's see the CGI args */
	if(FCA_search[0]!='\0') {
			/* search */
		if(!strcmp(FCA_search_dom, "all"))
			snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "cd ..;find %s", FCA_search);
		else
			snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "cd ../%s;find %s", FCA_search_dom, FCA_search);
		FCA_init_headers();
#ifdef CGI_DOWNLOADS
	} else if(FCA_must_download) {
			/* we download a file */
		pd=strchr(FCA_dir_to_list, ';');
		if(pd)	/* ahah, security alert */
			*pd='\0';
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "cat %s", FCA_dir_to_list);
		dw=true;
		FCA_init_download_headers();
#endif
	} else if(FCA_dir_to_list[0]!='\0') {
			/* list a directory */
		pd=strchr(FCA_dir_to_list, ';');
		if(pd)	/* ahah, security alert */
			*pd='\0';
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "ls %s", FCA_dir_to_list);
		FCA_init_headers();
	} else {	/* by default, list all domains */
		snprintf(buf, 5+FFSS_MAX_PATH_LENGTH, "ls /$");
		FCA_init_headers();
	}
	/* in CGI mode, we must redirect all error
	  messages to stdout, not to stderr (stderr is
	  redirected to the server web's log files)
	  all error messages before this header are redirected to
	  the log file.
	*/
	if(!dw)
		FCA_err_stream=stdout;
	FCA_command=buf;
/* to debug CGI scripts 
printf("<html>\n");
printf("command to run: '%s'<br>\n", FCA_command);
printf("</html>\n");
exit(0);
*/
	FCA_run_once(dw);
}
#endif

void FCA_first_list()
{
	    	/* first list domains */
	SU_PList Pdom;
    
	FCA_quiet=true;
	FCA_list_domains();
		/* pwd = /$/(first domain in list) */
	Pdom=FCA_Servers;
	if(Pdom==NULL)
		FCA_crash("internal error: empty domains");
	while(Pdom->Next!=NULL)
		Pdom=Pdom->Next;
	FFSS_PrintDebug(3, "(client) going to %s\n", ( (FCA_PServs)(Pdom->Data) )->Domain);
	FCA_strtoupper(( (FCA_PServs)(Pdom->Data) )->Domain);
	snprintf(FCA_pwd, sizeof(FCA_pwd), "/$/%s", ( (FCA_PServs)(Pdom->Data) )->Domain);
	SU_strcpy(FCA_home, ( (FCA_PServs)(Pdom->Data) )->Domain, FFSS_MAX_DOMAIN_LENGTH);
	FCA_quiet=false;
}

void FCA_uninit()
{
		/* free structure */
	FCA_free_Domains();
	FCA_free_Domain_list();
	FCA_free_list();
#ifdef ALIASES
	FCA_free_aliases();
#endif
#ifdef CGI
/* from libCGI
	cgi_end();
*/
#endif
	FCA_exiting=true;
	FCA_ShDisconnect();	/* disconnects if connected */
	if( !FC_UnInit() && !FCA_quiet)	/* cannot call FCA_crash... */
		printf("FATAL ERROR: cannot shutdown FFSS library !\n");
	FCA_restore_usr1();

	if(!FCA_quiet)
		FCA_prog_end();
}

/******************* main ******************/

int main(int argc, char **argv)
{
	FCA_init();
    
	FCA_args.argc=argc;
	FCA_args.argv=argv;
	FCA_get_args();

#ifdef CGI
	if(FCA_CGI_mode) {
		FCA_read_cfg();
/* debug
FCA_init_headers();
*/
		FCA_read_cgi_args();
		FCA_process_cgi_args();
	}
#endif

	FCA_process_args();	/* for debug level for example */
	FCA_read_cfg();
	FCA_process_args();	/* args can't be overwritten by the config */

	FCA_prog_begin();
	FCA_print_version();
	if( !FC_Init() )
		FCA_crash("cannot initialise FFSS client");
    
	FCA_first_list();

		/* we use exit() to quit */
	for(;;) {
		FCA_get_cmd();
		FCA_interpret_cmd();
	}
}
