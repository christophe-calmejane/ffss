/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	common funtions
 */

	/* just for threads */
#include <stdarg.h>
#include <unistd.h>
#include <signal.h>
#include <ffss.h>

#include "common.h"
#include "datastruct.h"
#include "client.h"
#include "display.h"
#include "config.h"
#include "command.h"
#include "cgi_args.h"
#include "skin.h"

	/* just for threads */
#include <unistd.h>
#include <signal.h>
#include <readline/readline.h>

	/* mkdir */
#ifndef _WIN32
#	include <sys/stat.h>
#endif

	/* for thread manipulation */
bool FCA_sem_locked;
bool FCA_wait_threading;
SU_SEM_HANDLE FCA_prmptSem;	/* the semaphore prompt <-> callbacks */
FFSS_PTransfer FCA_Ptrans;	/* the transfert pointer */
SU_THREAD_HANDLE FCA_wait_thread;

	/* for completion */
int FCA_compl_wanted;
bool FCA_sem_timeout;

	/* for logs */
FILE *FCA_logf;

	/* environment variables */
	/* WARNING for these 2 tables :
		if you modify the variable order,
		 check the index in cgi.c in the FCA_CGI_ARGS table
		if you add a variable, BE SURE to modify FCA_skin_env_index
		 in common.h
	*/
const FCA_Tvar FCA_VARS[]= {
/*
	variable's	description					values		on_var_change
	 name										 callback
*/
	{"color",	"color output",					"on/off",	NULL},
	{"debug_level",	"the debug level (0=no debug)",			"0-6",		FCA_upd_debuglevel},
	{"disp_off",	"if machines which are 'off' are diplayed",	"on/off",	NULL},
	{"disp_quiet",	"if machines which are 'quiet' are diplayed",	"on/off",	NULL},
	{"master",	"master's name or IP",				"name/IP",	NULL},
	{"prompt",	"if the user is prompted on mget/mput",		"on/off",	NULL},
	{"skin",	"the skin",					"name",		FCA_upd_skin},
	{"broadcast_timeout","timeout when listing domain None, in seconds","1-120",	NULL},
	{"search_timeout","timeout when searching in all domains, in seconds","1-120",	NULL},
	{"operation_timeout","timeout when doing something, in seconds","1-120",	NULL},
	{"sort_find",	"if the search answers must be sorted",		"on/off",	NULL},
	{"sort",	"if the listings must be sorted",		"on/off",	NULL},
	{"sort_by",	"sort method for directory listings",		"name/size/date",NULL},
	{"log",		"if we must log",				"on/off",	FCA_upd_log},
	{"log_file",	"the file used to log",				"filename",	FCA_upd_logfile},
	{"log_level",	"1=find 2=downloads 3=browsing 4=arguments 5=cgi 6=commands 7=all",	"1-6",	NULL},
	{"useconnsock",	"if we must use the same connection for downloads","on/off",	NULL},
	FCA_SKIN_VARS
	{NULL,		NULL,						NULL,		NULL}
};
	/* possible values to variables
	 WARNING: all strings here MUST NOT have a length > FCA_VAR_MAX
	  first field can be "" -> all values are accepted
	  after "", you can put purposed values for completion
	 */
const char *FCA_VAR_VALUES[][FCA_MAX_POSS_VALUES]= {
	{"on","off",NULL},
	{"0","1","2","3","4","5","6",NULL},
	{"on","off",NULL},
	{"on","off",NULL},
		/* here, all values are accepted, but a value can be the current master */
	{"", FCA_master,NULL},
	{"on","off",NULL},
	{FCA_SKINLIST, NULL},
	{"1", "2", "3", "5", "7", "10", "15", "20", "30", "40", "50", "60", "90", "120", NULL},
	{"1", "2", "3", "5", "7", "10", "15", "20", "30", "40", "50", "60", "90", "120", NULL},
	{"1", "2", "3", "5", "7", "10", "15", "20", "30", "40", "50", "60", "90", "120", "240", NULL},
	{"on","off",NULL},
	{"on","off",NULL},
	{"name", "size", "date", NULL},
	{"on","off",NULL},
	{"", "ffss-client.log", NULL},
	{"1", "2", "3", "4", "5", "6", "7", NULL},
	{"on","off",NULL},
	FCA_SKIN_VAR_VALUES
	{NULL}
};

	/* default value to variables */
char FCA_env[][FCA_VAR_MAX]={
	"on",
	"0",
	"on",
	"on",
	"",
	"off",
	"default",
	"5",
	"20",
	"10",
	"on",
	"on",
	"name",
	"off",
	"ffss-client.log",
	"3",
	"off",
	FCA_SKIN_ENV_VALUES
};



int FCA_RequestDownload(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[], FFSS_LongField size)
{
	FILE *fd;
	FFSS_LongField oldsize;
	FFSS_LongField start;
		/* TODO: and parameter 'use same thread ?' (=true) */
	FCA_Ptrans = NULL;
	start=0;
	
		/* only if it's not a cat */
	if(LocalPath && LocalPath[0]!='\0') {
			/* if same file exist (replace || resume ?) */
		fd=fopen(LocalPath, "r");
		if(fd) {
			FFSS_PrintDebug(1, "file already exists\n");
			fseek(fd,0,SEEK_END);
			oldsize=ftell(fd);
			fclose(fd);
			if(oldsize==size) {	/* file exist, same size */
				printf("file %s already exists (with the same size), download it ", LocalPath);
				if(!FCA_question("again") )
					return -1;
			} else if(oldsize<size) {	/* exists, need ot resume */
				FCA_print_info("resuming download...");
				start=oldsize;
			} else {			/* exists, but bad size */
				printf("file %s already exists (bigger size), download it ", LocalPath);
				if(!FCA_question("again") )
					return -1;
			}
		} else
			FFSS_PrintDebug(1, "(client) local file doesn\'t exists, cannot resume\n");
	}
	if(FCA_VAR_IS_ON(FCA_prompt)) {
		printf("download file ");
		if( FCA_question(RemotePath) )
			return !FFSS_DownloadFile(Server,RemotePath,LocalPath,start,NULL,FCA_useConnSock[1]=='n',&FCA_Ptrans);
		else		/* unlike an error if we answer 'n' */
			return -1;
	} else
		return !FFSS_DownloadFile(Server,RemotePath,LocalPath,start,NULL,FCA_useConnSock[1]=='n',&FCA_Ptrans);
}

unsigned short int FCA_ShConnect(char *IP, char *share, char *login, char *passwd)
{
	/* returns 0 on success, 1 if cannot connect, 2 if already connected */
	
	if( FCA_conSh[0]!='\0'
	    && !SU_strcasecmp(share, FCA_conSh)
	    && !SU_strcasecmp(IP, FCA_conIP))	/* already connected to the same share */
		return 2;

	if( FCA_conSh[0]!='\0' ) {	/* if connected */
		FCA_canChange_pwd=false;	/* but don't change pwd... */
		FCA_ShDisconnect();
		FCA_canChange_pwd=true;
	}
		/******* todo: login/pass ******/
	FCA_shrSkt=FC_SendMessage_ShareConnect(IP, share, login, passwd);
	if(FCA_shrSkt==NULL)
		FCA_print_err("cannot connect to this share");
	else {
			/* update conSh && conIP */
		SU_strcpy(FCA_conSh, share, FFSS_MAX_PATH_LENGTH);
		SU_strcpy(FCA_conIP, IP, FFSS_IP_FIELD_SIZE);
	}
	return( FCA_shrSkt==NULL );
}

bool FCA_list_servs(const char *domain)
{
	FCA_UDP_errno=0;
	FCA_posted=false;
	if( FCA_master[0]=='\0' || !SU_strcasecmp(domain, "None") || ! FC_SendMessage_ServerList(FCA_master,NULL,domain) ) {
		FCA_posted=false;
	    	FCA_broadcast();
	        return false;
	}
	if(!FCA_posted)
	    	FCA_sem_wait();
	if(FCA_UDP_errno!=0) {
		FCA_print_cmd_err("cannot contact master...using broadcast");
		FCA_broadcast();
	}
	return false;
}

void FCA_list_domains()
{
	FCA_posted=false;
	FCA_everListDom=true;
	FCA_UDP_errno=0;
	if(! FC_SendMessage_DomainListing(FCA_master) ) {
		FCA_print_warning("cannot get domain listing");
		FCA_print_domains(NULL,0);
	} else {
    		/* we must wait after... */
		if(!FCA_posted)
			FCA_sem_wait();
		if(FCA_UDP_errno)
			FCA_print_domains(NULL,0);
		FFSS_PrintDebug(1, "(client) domains listed\n");
	}
	FCA_posted=false;
}

void FCA_broadcast()
{
		/* uses broadcast...we uses timeout */
	FCA_UDP_errno=0;
		/* prepare the listing structure */
	SU_strcpy(FCA_listed_dir, "/$/none", FFSS_MAX_FILEPATH_LENGTH);
	FCA_free_list();
	FCA_list=NULL;
	FCA_nb_states=0;
	FCA_inDispServs=true;
        if( FC_SendMessage_ServerSearch() ) {
		sleep(FCA_broadcast_timeout);	/* sleep and post after */
		FCA_inDispServs=false;		/* after this, all answers are ignored */
		FCA_print_post_states();
	} else
		FCA_print_cmd_err("cannot broadcast...");
}

void FCA_close_connection()
{
	/* disconnects, this socked is altered or empty */
	FCA_shrSkt=NULL;
	FCA_conSh[0]='\0';
	FCA_conIP[0]='\0';
}

void FCA_ShDisconnect()
{
	if(FCA_shrSkt!=NULL)	/* already connected to an other share */
	{
		FCA_conIP[0]='\0';
		FCA_conSh[0]='\0';
		FCA_posted=false;
		FC_SendMessage_Disconnect(FCA_shrSkt);
		if(!FCA_posted)
			FCA_sem_wait();
		FCA_posted=false;
	}
}

void FCA_exit(int code)
{
	if(!FCA_exiting) {
		if(FCA_logf) {
			FCA_printlog("logging stopped");
			SU_CloseLogFile(FCA_logf);
			FCA_logf=NULL;
		}
		FCA_uninit();
		exit(code);
	}
}

    /* some tests */
FM_PHost isMachineValid(char *machine, char *domain)
{
		/* return reference in the strcture if the server is valid */
	SU_PList Pservs;
    
		/* look for this machine in the structure */
		/* first look for domain */
	Pservs=FCA_Servers;
	while(Pservs!=NULL && SU_strcasecmp( ((FCA_PServs)(Pservs->Data))->Domain, domain) )
		Pservs=Pservs->Next;
	if(Pservs==NULL)	/* arg, domain not found */
		return NULL;
	Pservs=((FCA_PServs)(Pservs->Data))->servers;
		/* now we can look for this server */
	while(Pservs!=NULL && SU_strcasecmp( ((FM_PHost)(Pservs->Data))->Name, machine) )
		Pservs=Pservs->Next;
	if(Pservs==NULL)	/* urg! server not found */
		return NULL;
	return ((FM_PHost)(Pservs->Data));
}

bool isShareValid(char *IP, char *share)
{
		/* if this share is valid, we are connected */
	unsigned short int res;
    
	FCA_err_errno=FFSS_ERROR_NO_ERROR;
	FCA_posted=false;
		/* todo: login/pass */
	res=FCA_ShConnect(IP, share, NULL, NULL);
	if(!res && !FCA_posted) {	/* new connection */
	    	FFSS_PrintDebug(1, "(client::isShValid) waiting\n");
	        FCA_sem_wait();
	}
	FCA_posted=false;
	if(res==1)
	        return false;
	if( FCA_err_errno!=FFSS_ERROR_NO_ERROR) {
		FCA_err_errno=FFSS_ERROR_NO_ERROR;
		return false;
	}
	return true;
}

bool isDirValid(char *path, char *dir)
{
	FCA_quiet=true;
	FCA_err_errno=FFSS_ERROR_NO_ERROR;
		/* dir -> /.... */
	FCA_posted=false;
	if(! FCA_list_dir(FCA_shrSkt, path, dir))
		return false;
	else {
	    	if(!FCA_posted)
			FCA_sem_wait();
	
		FCA_quiet=false;
		if( FCA_err_errno!=FFSS_ERROR_NO_ERROR) {
			FCA_err_errno=FFSS_ERROR_NO_ERROR;
			return false;
		}
	}
	return true;
}

	/* thread manipulation */
void FCA_sem_init()
{
	/*
	sem_init(&FCA_prmptSem,0,0);	
	*/
	SU_CreateSem(&FCA_prmptSem, 0, 0, "promptsem");
	
	FCA_sem_locked=false;
	FCA_wait_threading=false;
}

void FCA_sem_wait_no_timeout()
{
		/* a sem wait with no timeout, useful for downloads */
	FFSS_PrintDebug(1, "(client) FCA_sem_wait_no_timeout started\n");

	if(FCA_sem_locked)
		FFSS_PrintDebug(1, "(client) sem has ever been locked\n");
	else {

		FFSS_PrintDebug(1, "(client) FCA_sem_no_timeout waiting\n");
	
		if(FCA_sem_locked)
			FFSS_PrintDebug(1, "(client) sem has ever been locked\n");
		else {
			FCA_sem_locked=true;
			FCA_wait_threading=true;
			SU_SEM_WAIT(FCA_prmptSem);
		}
	}
}


void FCA_sem_wait()
{
	FFSS_PrintDebug(1, "(client) FCA_sem_wait started\n");

	if(FCA_sem_locked)
		FFSS_PrintDebug(1, "(client) sem has ever been locked\n");
	else {

		FFSS_PrintDebug(1, "(client) FCA_sem waiting\n");
	
		if(FCA_sem_locked)
			FFSS_PrintDebug(1, "(client) sem has ever been locked\n");
		else {
			FCA_sem_locked=true;
			FFSS_PrintDebug(1, "(client) launching timer\n");
			FCA_wait_threading=true;
			if( ! SU_CreateThread(&FCA_wait_thread, FCA_sem_timer, NULL, true) )
				FFSS_PrintDebug(1,"(client) cannot create wait timer thread!\n");
			else
				FFSS_PrintDebug(1, "(client) timer thread created and detached\n");
			FFSS_PrintDebug(1, "(client) timer launched, waiting...\n");
			SU_SEM_WAIT(FCA_prmptSem);
			FFSS_PrintDebug(1, "(client) sem_wait unlocked.\n");
		}
	}
}


#ifdef _WIN32
void FCA_sem_timer()
#else
void *FCA_sem_timer()
#endif
{
	FFSS_PrintDebug(1, "(client) preparing timer\n");
	if(!FCA_sem_locked) {
		FFSS_PrintDebug(1, "(client) no need of timer\n");
		SU_END_THREAD(0);
	}
		/* we mustn't ignore the SIG_USE1 signal */
	FCA_restore_usr1();
	signal(SIGUSR1, FCA_timer_handusr1);
	FFSS_PrintDebug(1, "(client) timer launched for %d seconds\n", FCA_operation_timeout);
	sleep(FCA_operation_timeout);
		/* now we must ignore SIG_USR1 */
	FCA_ignore_usr1();

	if(FCA_sem_locked) {
		FCA_sem_timeout=true;
		FFSS_PrintDebug(1, "(client) sem timeout...\n");
		FCA_print_err("operation timeout");
		if(FCA_sem_locked) {
			FFSS_PrintDebug(1, "(client) timer is helping you by using sem_post !!\n");
			SU_SEM_POST(FCA_prmptSem);
		}
		FCA_sem_locked=false;
	} else
		FFSS_PrintDebug(6, "(client) timer dead\n");
	SU_END_THREAD(0);
}

void FCA_timer_handusr1()
{
	FFSS_PrintDebug(1, "(client) timer killed\n");
	SU_END_THREAD(0);
}


void FCA_sem_post()
{
	FFSS_PrintDebug(1, "(client) FCA_sem_post started\n");
	
		/* posted is if we have tried to post */
	FCA_posted=true;
	if(!FCA_sem_locked)
		FFSS_PrintDebug(1, "(client) sem has ever been unlocked\n");
	else {
			/* kill timer thread */
		if(FCA_wait_threading) {

			FFSS_PrintDebug(1, "(client) killing timer\n");
			pthread_kill(FCA_wait_thread, SIGUSR1);

			FCA_wait_threading=false;
		}

		FCA_sem_locked=false;
		FFSS_PrintDebug(1, "(client) sem_post\n");
		SU_SEM_POST(FCA_prmptSem);
	}
}

void FCA_ignore_usr1()
{
#ifdef __unix__
	sigset_t mask;
	
	sigemptyset(&mask);
		/* ignore only SIG_USR1 */
	sigaddset(&mask, SIGUSR1);
	pthread_sigmask(SIG_BLOCK,&mask,NULL);
#endif
}

void FCA_restore_usr1()
{
#ifdef __unix__
	sigset_t mask;
	
	sigemptyset(&mask);
	sigaddset(&mask, SIGUSR1);
	pthread_sigmask(SIG_UNBLOCK,&mask,NULL);
#endif
}


    /* completion */
char **FCA_completion(char *text, int start, int end)
{
	/* algorithm adapted from GNU readline documentation */
    char **matches;
    char first_comm[FCA_MAX_CMD];
    char *fsp;
    unsigned int iC;
    
	/* if we are typing a command */
    if(!start)
	matches = rl_completion_matches (text, (void*)FCA_cmd_gen);
    else {
		/* get the command name */
	SU_strcpy(first_comm, rl_line_buffer, FCA_MAX_CMD);
	first_comm[FCA_MAX_CMD-1]='\0';
	fsp=strchr(first_comm, ' ');
	if(fsp!=NULL)
		*fsp='\0';

	matches = (char **)NULL;
		/* look for the command in the table */
	iC=0;
	while( FCA_COMMANDS[iC].name!=NULL && SU_strcasecmp(FCA_COMMANDS[iC].name, first_comm) )
		iC++;
	if(FCA_COMMANDS[iC].name) {
		FCA_compl_wanted=FCA_COMMANDS[iC].arg_type;
		if( FCA_COMMANDS[iC].arg_type & FCA_CMD_ARG )
			matches = rl_completion_matches (text, (void*)FCA_cmd_gen);
		else if( FCA_COMMANDS[iC].arg_type & FCA_VAR_ARG)
			matches = rl_completion_matches (text, (void*)FCA_var_gen);
		else if( FCA_COMMANDS[iC].arg_type & FCA_ALL_ARG
		 || FCA_COMMANDS[iC].arg_type & FCA_DIR_ARG )
			matches = rl_completion_matches (text, (void*)FCA_path_gen);
		else if( (FCA_COMMANDS[iC].arg_type & FCA_LOCAL_ARG)==0) /* no completion */
			rl_attempted_completion_over=true;
		/* else, local completion (default) */
	}

#ifdef ALIASES
		/* no completion for aliases */
	if( FCA_get_alias(first_comm) )
#endif
		rl_attempted_completion_over=true;
    }
    return (matches);
}

char *FCA_cmd_gen(char *text, int state)
{
		/* compare with commands to complete the word */
	static int list_index, len;
	static bool compl_var;
#ifdef ALIASES
	static SU_PList pl;
#endif
	char *ws, *ws2, cmd[FCA_MAX_CMD];
	unsigned int iC;
#ifdef ALIASES
	char *p;
#endif

		/* new word to complete */
	if (!state) {
		list_index = 0;
		compl_var=false;
#ifdef ALIASES
		pl=FCA_aliases;
#endif
		len = strlen (text);
		rl_completion_append_character=(int)' ';
	}
	
	if(compl_var)
		return FCA_var_gen(text,state);
	
	ws=strchr(rl_line_buffer, ' ');
	if(ws) {
		ws2=strchr(ws+1, ' ');
			/* we are at the third argument */
		if(ws2) {
			*ws2='\0';
				/* if the completed command accepts a variable */
			SU_strcpy(cmd, ws+1, FCA_MAX_CMD);
			cmd[FCA_MAX_CMD-1]='\0';
			
			if( strchr(ws2+1, ' ')!=NULL ) {
				*ws2=' ';
				rl_attempted_completion_over=true;
				return NULL;
			}
				/* look for the command in the table */
			iC=0;
			while( FCA_COMMANDS[iC].name!=NULL && SU_strcasecmp(FCA_COMMANDS[iC].name, cmd) )
				iC++;
			if(FCA_COMMANDS[iC].name!=NULL &&
			 (FCA_COMMANDS[iC].arg_type & FCA_VAR_ARG) ) {
			 	*ws2=' ';
			 	compl_var=true;
				return FCA_var_gen(text,state);
			}
			*ws2=' ';
			rl_attempted_completion_over=true;
			return NULL;
		}
	}

		/* Return the next name which partially matches from the command list. */
	while ( NULL!=FCA_COMMANDS[list_index].name ) {
		list_index++;
		if( !strncasecmp(FCA_COMMANDS[list_index-1].name, text, len) && FCA_COMMANDS[list_index-1].name[0]!='\0' ) {
			return strdup(FCA_COMMANDS[list_index-1].name);
		}
	}

#ifdef ALIASES
		/* after, there are aliases */
	while(pl) {
		p=((FCA_Palias)(pl->Data))->cmd;
		pl=pl->Next;
		if( !strncasecmp(p, text, len) )
			return strdup(p);
	}
#endif
		/* If no names matched, then return NULL. */
	rl_attempted_completion_over=true;
	return NULL;
}

char *FCA_path_gen(char *text, int state)
{
		/* compare with commands to complete the word */
	static int len;
	static char *ws;	/* word start */
	static char *rtext;
	static bool started;
	static SU_PList Pl;		/* pointer to the structure listing */
	char *tolist;

		/* new word to complete */
	if (!state) {
			/*********** for the moment, it's a simple completion ************/
		started=false;
		if( !strcmp(text,"/") )
			return strdup("/$");
		if( !strcmp(text,"/$") )
			return strdup("/$");
		if( !strncmp(text,"/$/",3) )
			return strdup("/$");
		len=strlen(text);
			/* get the begin of the word */
			/* we suppose that it's only commands with one argument */
		ws=strchr(rl_line_buffer,' ');
		if(ws)
			ws++;
		else
			ws=text;
			/* it's just text, but pointer to rl_line_buffer */
		rtext=strrchr(rl_line_buffer,' ');
		if(rtext)
			rtext++;
		else
			rtext=rl_line_buffer;
		
		rl_completion_append_character=(int)' ';
			/* get the good directory */
		if(*rtext=='/')		/* absolute path */
			tolist=rtext;
		else			/* relative path */
			tolist=FCA_pwd;
			/* see if we need to list a directory */
		if( SU_strcasecmp(tolist,FCA_listed_dir) ) {	/* yes, we need */
				/* send the listing */
			FFSS_PrintDebug(3, "(client) need to list to complete\n");
			FCA_quiet=true;
			if( FCA_ls_cmd(tolist) && !FCA_posted) {
				FFSS_PrintDebug(1, "(client) message sent...waiting\n");
				FCA_sem_wait();
			}
			FCA_quiet=false;
				/* if there's an error....return NULL */
			if( SU_strcasecmp(tolist,FCA_listed_dir) ) {
				FFSS_PrintDebug(1, "(client) '%s'!='%s'\n",tolist,FCA_listed_dir);
				FFSS_PrintDebug(3, "(client) we've not the listing...there was an error\n");
				rl_attempted_completion_over=true;
				return NULL;
			}
		}
		Pl=FCA_list;
		
	}
	if(!started) {
		if( !strcmp(text,"/")
		 || !strcmp(text,"/$")
		 || !strncmp(text,"/$/",3) ) {
		 	rl_completion_append_character=(int)'/';
			rl_attempted_completion_over=true;
			return NULL;
		}
			/* don't purpose ".." if the first letter isn't at least '.' */
		if(len && !strncmp(ws, "..", rtext-ws+len) ) {
			if(!state) {
				rl_completion_append_character=(int)'/';
				return strdup("..");
			} else
				started=true;
		} else
			started=true;
	} else if(state)	/* if started, read the next */
		Pl=Pl->Next;
		/* Return the next name which partially matches from the command list. */
	while(Pl) {
		if(!strncasecmp( ((FC_PEntry)(Pl->Data))->Name, ws, rtext-ws+len )) {
				/* it's a dir, add a '/' */
			if( ((FC_PEntry)(Pl->Data))->Flags & FFSS_FILE_DIRECTORY )
				rl_completion_append_character=(int)'/';
			if( (((FC_PEntry)(Pl->Data))->Flags & FFSS_FILE_DIRECTORY)!=0
			 || FCA_compl_wanted!=FCA_DIR_ARG) {
				return strdup(( ((FC_PEntry)(Pl->Data))->Name+(rtext-ws) ));
			}
		}
		Pl=Pl->Next;
	}
		/* If no names matched, then return NULL. */
	rl_attempted_completion_over=true;
	return NULL;
}

char *FCA_var_gen(char *text, int state)
{
		/* compare with variables to complete the word */
	static int list_index, len;
	static bool compl_val;
	char *ws, *ws2, cmd[FCA_MAX_CMD];
	unsigned int iC;
	
		/* new word to complete */
	if (!state) {
		list_index = 0;
		compl_val=false;
		len = strlen (text);
		rl_completion_append_character=(int)' ';
	}
	
	if(compl_val)
		return FCA_val_gen(text,state);
	
	ws=strchr(rl_line_buffer,' ');
		/* we accept val completion only if we're on the third argument */
	if(ws) {
		ws2=strchr(ws+1, ' ');
			/* 4th+ arg */
		if(ws2 && strchr(ws2+1, ' ')) {
			rl_attempted_completion_over=true;
			return NULL;
		}
		if(ws2) {
			*ws='\0';
			SU_strcpy(cmd, rl_line_buffer, FCA_MAX_CMD);
			*ws=' ';
				/* look for the command */
			iC=0;
			while( FCA_COMMANDS[iC].name!=NULL && SU_strcasecmp(FCA_COMMANDS[iC].name, cmd) )
				iC++;
			if(FCA_COMMANDS[iC].name!=NULL &&
			 !(FCA_COMMANDS[iC].arg_type & FCA_CMD_ARG) ) {
				compl_val=true;
				return FCA_val_gen(text,state);
			}
		}
	}
	
	
		/* Return the next name which partially matches from the command list. */
	while ( NULL!=FCA_VARS[list_index].name ) {
		list_index++;	
		if( !strncasecmp(FCA_VARS[list_index-1].name, text, len) )
			return strdup(FCA_VARS[list_index-1].name);
	}

		/* If no names matched, then return NULL. */
	rl_attempted_completion_over=true;
	return NULL;
}

char *FCA_val_gen(char *text, int state)
{
		/* compare with variables to complete the word */
	static int list_index, len;
	static int ivar;
	char *fsp, var[FCA_MAX_CMD+1+FCA_VAR_MAX], *fsp2;
	
		/* new word to complete */
	if (!state) {
		list_index = 0;
		len = strlen (text);
		rl_completion_append_character=(int)' ';
			/* we must find the variable */
		SU_strcpy(var, rl_line_buffer, FCA_MAX_CMD+FCA_VAR_MAX+1);
		var[FCA_MAX_CMD+FCA_VAR_MAX]='\0';
		fsp=strchr(var, ' ');
		if(!fsp) {	/* damned */
			rl_attempted_completion_over=true;
			return NULL;
		}
		fsp2=strchr(fsp+1, ' ');
		if( fsp2!=NULL )
			*fsp2='\0';
			

			/* look for the variable in the table */
		ivar=0;
		while( FCA_VARS[ivar].name!=NULL && SU_strcasecmp(FCA_VARS[ivar].name, fsp+1) )
			ivar++;
		if( FCA_VARS[ivar].name==NULL ) {
			rl_attempted_completion_over=true;
			return NULL;
		}
	}
	
	
		/* Return the next name which partially matches from the command list. */
	while ( NULL!=FCA_VAR_VALUES[ivar][list_index] ) {
		list_index++;
		if( FCA_VAR_VALUES[ivar][list_index-1][0]!='\0' &&
		 !strncasecmp(FCA_VAR_VALUES[ivar][list_index-1], text, len) )
			return strdup(FCA_VAR_VALUES[ivar][list_index-1]);
	}

		/* If no names matched, then return NULL. */
	rl_attempted_completion_over=true;
	return NULL;
}



    /* string manipulation */
	/* encode-decode: just if we need: replaces ' ' by '\ ', etc... 
	 it was used for completion
	*/
char *FCA_new_encoded(char *txt)
{
		/* this special caracters are listed by rl_basic_word_break_characters */
	char *sp_cars;
	char *p;
	char *res, *pr;
	int nbsup=0;	/* the nb of caracters we need in supplement */
	int size=0;
	
	p=txt;
		/* find nbsup */
	while(*p) {
		sp_cars=rl_basic_word_break_characters;
		while(*sp_cars)
			nbsup+=( *p==*(sp_cars++) );
		p++;
		size++;
	}
		/* ok, we now know the size we need */
	res=malloc( sizeof(char)*(size+nbsup+1) );
		/* encode all */
	p=txt;
	pr=res;
	while(*p) {
		sp_cars=rl_basic_word_break_characters;
		while(*sp_cars) {
			if( *p==*sp_cars ) {
				*(pr++)='\\';
				break;
			}
			sp_cars++;
		}
		*(pr++)=*p;
		p++;
	}
	*(pr++)='\0';
	return res;
}

void FCA_decode(char *txt)
{
		/* translate '\ ' to ' ' for example */
	char *p=txt;
	int decal=0;
	
	while( *(p+decal) ) {
		if( *(p+decal)=='\\' )
			decal++;
		*p=*(p+decal);
		p++;
	}
		/* for the \0 */
	*p=*(p+decal);
}

void FCA_del_lsp(char *cmd)
{
		/* delete last spaces(and tabs) of the command */
	char *lsp;

	if(cmd==NULL || *cmd=='\0')	/* no command */
		return;
	lsp=cmd+strlen(cmd)-1;
	if( *lsp!=' ' && *lsp!='\t' )	/* no last spaces */
		return;
	if(lsp==cmd) {	/* cmd = ' ' || '\t' */
		*cmd='\0';
		return;
	}

	while(lsp>cmd && (*(--lsp)==' ' || *lsp=='\t') );
	if( *(lsp+1)==' ' || *(lsp+1)=='\t' )
		*(lsp+1)='\0';
}

char *FCA_del_fsp(char *cmd)
{
		/* delete first spaces (and tabs) of the command */
	char *sp;

	if(cmd==NULL || *cmd=='\0')	/* no command */
		return cmd;
	sp=cmd;
	if( *sp!=' ' && *sp!='\t' )	/* no first spaces */
		return cmd;

	while( *(++sp)==' ' || *sp=='\t' );
	return sp;
}


bool FCA_explode_path(char *path, char **domain, char **machine, char **share, char **dir)
{
	/* from a path like /$/fleming/orion/etc/X11/XF86Config, get:
	 *	the domain: 	fleming
	 *	the machine: 	orion or orion's IP if orion is found in the structure
	 *	the share:	etc
	 *	the dir:	X11/XF86Config
	 *  WARNING: it modify directly the path variable, save this before if you want
	 */

    char *Ppath=path;
    *domain=NULL;*machine=NULL;*share=NULL;*dir=NULL;

    if(path==NULL)
	return false;
    if( *Ppath!='/' || *(Ppath+1)!='$' )
	return false;
    if( *(Ppath+2)!='/'  )	/* it's /$ or an error */
    	return *(Ppath+2)=='\0';
    Ppath+=3;
    if(Ppath=='\0')
	return false;
    *domain=Ppath;
    Ppath=strchr(Ppath, '/');
    if(Ppath==NULL || *(Ppath+1)=='\0')	/* just the domain... */
	return true;
    *(Ppath++)='\0';	/* end of domain ('/'->'\0') */
    *machine=Ppath;
    Ppath=strchr(Ppath, '/');
    if(Ppath==NULL || *(++Ppath)=='\0')	/* just domain & server */
	return true;
    *(Ppath-1)='\0';	/* end of machine ('/'->'\0') */
    *share=Ppath;
    Ppath=strchr(Ppath, '/');
    if(Ppath==NULL)	/* just domain & machine & share */
	return true;
    *Ppath='\0';	/* end of share ('/'->'\0') */
    *dir=Ppath+1;
    return true;
}    

void FCA_process_pp(char *path)
{
	/* eliminates .. in paths, like /$/fleming/bennyben/test/toto/../test/.. */
    char *ptpt;
    char *PpthS;
    char *PpthD;
    
    if(path==NULL || *path=='\0')
	return;
    ptpt=strchr(path, '.');
    while( ptpt!=NULL && *ptpt!='\0' ) {
    	if( *(ptpt+1)=='.'  && ptpt>path+1 && *(ptpt-1)=='/' && *(ptpt+1)!='\0' && ( *(ptpt+2)=='/' || *(ptpt+2)=='\0') ) {
	    PpthD=ptpt-2;
	    while(PpthD>=path+1 && *PpthD!='/')	/* find precedent / */
		PpthD--;
	    if(*PpthD=='/') {
		PpthS=ptpt+2;
		if( PpthD<=path )		/* if /.. */
		    PpthD=ptpt-1;
		ptpt=PpthD-1;
		*PpthD=*PpthS;
		do {
		    *(++PpthD)=*(++PpthS);
		} while( *PpthS!='\0' );		/* << all the substring && the \0 */
	    } else
		break;
	}
	if( *ptpt=='\0' && ptpt>path )
	    ptpt--;
	ptpt=strchr(ptpt+1, '.');
    }
    ptpt=path+strlen(path)-1;
	/* delete / on the end */
    while( *ptpt=='/' )
	    *(ptpt--)='\0';
}

char *FCA_strtoupper(char *str)
{
		/* convert this string to uppercase
		  WARNING: this string is directly modified
		*/
	char *Pstr=str;
	
	if(str==NULL)	return str;
	
	while(*Pstr) { 
		*Pstr=toupper(*Pstr);
		Pstr++;
	}
	return str;
}

char *FCA_strtolower(char *str)
{
		/* convert this string to lowercase
		  WARNING: this string is directly modified
		*/
	char *Pstr=str;
	
	if(str==NULL)	return str;
	
	while(*Pstr) { 
		*Pstr=tolower(*Pstr);
		Pstr++;
	}
	return str;
}

	/* SORTING */

unsigned int *FCA_pre_tabsort(int nbEl)
{
	unsigned int ir;
	unsigned int *res;
	
		/* prepare the int array */
	if(nbEl<1)
		return NULL;
	FFSS_PrintDebug(1, "(client) sorting...\n");
	FFSS_PrintDebug(5, "(client) preparing index table [0->%d]\n", nbEl-1);
		/* prepare the index table */
	res=malloc(nbEl*sizeof(int));
	if(!res)
		FCA_crash("no such memory");
	FFSS_PrintDebug(5, "(client) filling index table...\n");
		/* fill this */
	for(ir=0; ir<nbEl; ir++)
		res[ir]=ir;
	return res;
}

void FCA_sort_chartab(unsigned int *res, const char **els,int Nbels)
{
		/* sort search results */
	bool modified=true;
	unsigned int ir, sav;
	
	if(Nbels<2) {
		FFSS_PrintDebug(1, "(client) 1 element, nothing to sort\n");
		return;
	}
	FFSS_PrintDebug(1, "(client) let's sort...\n");
		/* while we have something to sort */
	while(modified) {
			/* first, from left to right */
		for(ir=0; ir<Nbels-1; ir++) {
				/* answer > next answer.... */
			if( strcmp(*(els+res[ir]), *(els+res[ir+1]))>0 ) {
				sav=res[ir];
				res[ir]=res[ir+1];
				res[ir+1]=sav;
				modified=true;
			}
		}
		if(modified) {
			modified=false;
				/* second (if needed), from right to left */
			for(ir=Nbels-2; ir>0; ir--) {
					/* answer > next answer.... */
				if( strcmp(*(els+res[ir]), *(els+res[ir+1]))>0 ) {
					sav=res[ir];
					res[ir]=res[ir+1];
					res[ir+1]=sav;
					modified=true;
				}
			}
		}
	}
	FFSS_PrintDebug(1, "(client) answers sorted\n");
	return;
}


void FCA_dw_dir(char *path, char *dir, char *dest)
{
		/* download recursively a directory */
	SU_PList P, current_list;
	FC_PEntry E;
	char *ndest, *next, *sav;
	unsigned int lg;
	char tgt[FFSS_MAX_PATH_LENGTH];
	int code;
	
	FFSS_PrintDebug(5, "(client) downloading recursively directory %s...\n", dir);
		/* fist get the listing */
	if( strcmp(dir,FCA_listed_dir) ) {
			/* if the listing is not already in the cache */
		FCA_posted=false;
		FCA_list_dir(FCA_shrSkt, path, dir);
		if(!FCA_posted)
			FCA_sem_wait();
		FCA_posted=false;
	}
		/* browse the list structure */
	P=FCA_list;
	current_list=FCA_list;
		/* to preserve the listing, we must do this
		   otherwise, the structure will be freed
		 */
	FCA_list=NULL;
	sav=strdup(FCA_listed_dir);
	FCA_listed_dir[0]='\0';
	FCA_err_errno=FFSS_ERROR_NO_ERROR;
	while(P && FCA_err_errno==FFSS_ERROR_NO_ERROR) {
		E=(FC_PEntry)(P->Data);
			/* it's a folder */
			/* ndest: the local destination */
		lg=strlen(dest)+1+strlen(E->Name)+1;
		ndest=malloc( lg*sizeof(char) );
		sprintf(ndest,"%s/%s", dest, E->Name);
			/* next: an other element to download */
		lg=strlen(dir)+1+strlen(E->Name)+1;
		next=malloc( lg*sizeof(char) );
		sprintf(next,"%s/%s",dir,E->Name);
			/* if this element is a directory */
		if( E->Flags & FFSS_FILE_DIRECTORY ) {
			FCA_mkdir(ndest);
			sprintf(tgt,"%s/%s",path,E->Name);
			FCA_dw_dir(tgt, next, ndest);
		} else {		/* it's a file */
			FCA_posted=false;

			code=FCA_RequestDownload(FCA_shrSkt, next, ndest, E->Size);
			if(code>0)
				FCA_print_cmd_err("Download failed");
			else if(!code) {
				if(! FCA_posted) {
					FFSS_PrintDebug(5, "(client) waiting transfert of %s\n",E->Name);
					FCA_sem_wait_no_timeout();
					FFSS_PrintDebug(5, "(client) transfert ended for file %s\n",E->Name);
				}
			}
		}
		free(next);
		free(ndest);
		P=P->Next;
	}
	if(FCA_err_errno!=FFSS_ERROR_NO_ERROR)
		FCA_print_warning("download aborded anormally, stopping...");
		/* free the current listing */
	P=FCA_list;
	FCA_list=current_list;
	FCA_free_list();
	FCA_list=P;
	strcpy(FCA_listed_dir, sav);
}


bool FCA_list_dir(SU_PClientSocket Server,const char Path[], const char Dir[])
{
		/* just send the message */
	SU_strcpy(FCA_tolist_dir, Path, FFSS_MAX_FILEPATH_LENGTH);
	return FC_SendMessage_DirectoryListing(Server,Dir);
}

bool FCA_list_shares(const char Path[], const char Server[])
{
		/* just send the message */
	SU_strcpy(FCA_tolist_dir, Path, FFSS_MAX_FILEPATH_LENGTH);
	return FC_SendMessage_SharesListing(Server);
}

bool FCA_if_same_share(char *path, char *path2)
{
		/* compare the 2 paths and say if it's the same server/domain/share */
	char *p=path, *p2=path2;
	int i;
	
		/* first goto the 4th / */
	for(i=0; i<3; i++) {
		p=strchr(p+1, (int)'/');
		p2=strchr(p2+1, (int)'/');
		FFSS_PrintDebug(1, "(client) state %s,%s\n",p,p2);
		if(!p || !p2)
			return false;
	}
	
	p=strchr(p+1, (char)'/');
	p2=strchr(p2+1, (char)'/');
		/* let's compare */
	if(p)
		*p='\0';
	if(p2)
		*p2='\0';
	i=SU_strcasecmp(path,path2);
	FFSS_PrintDebug(1, "(client) %s<->%s -> %d\n",path,path2, i);
	if(p)
		*p='/';
	if(p2)
		*p2='/';	/* restore the string */
	return i?false:true;
}

bool FCA_if_same_server(char *path, char *path2)
{
		/* compare the 2 paths and say if it's the same server/domain */
	char *p=path, *p2=path2;
	int i;
	
		/* first goto the 3th / */
	for(i=0; i<2; i++) {
		p=strchr(p+1, (int)'/');
		p2=strchr(p2+1, (int)'/');
		FFSS_PrintDebug(1, "(client) state %s,%s\n",p,p2);
		if(!p || !p2)
			return false;
	}
	
	p=strchr(p+1, (char)'/');
	p2=strchr(p2+1, (char)'/');
		/* let's compare */
	if(p)
		*p='\0';
	if(p2)
		*p2='\0';
	i=SU_strcasecmp(path,path2);
	FFSS_PrintDebug(1, "(client) %s<->%s -> %d\n",path,path2, i);
	if(p)
		*p='/';
	if(p2)
		*p2='/';	/* restore the string */
	return i?false:true;
}


void FCA_get_lpwd(char *dir)
{
	if(!getcwd(dir, FFSS_MAX_FILEPATH_LENGTH)) {
		dir[0]='\0';
		FCA_print_cmd_err("cannot get current local directory");
	}
}

bool FCA_if_val_ok(int i, const char *value)
{
	int i2;
	bool ok;
		/* verify if the value of the variable is correct */
	if(FCA_VAR_VALUES[i][0][0]=='\0')
		return true;
		/* we can't assign any value to this variable */
	i2=0;
	ok=false;
	while(FCA_VAR_VALUES[i][i2]!=NULL && !ok) {
		if( !SU_strcasecmp(FCA_VAR_VALUES[i][i2], value) )
			ok=true;
		i2++;
	}
	return ok;
}

void FCA_printlog(char *msg, ...)
{
		/* printf for logs */
	va_list argptr;
	char strtxt[1005];
	char strIP[32]="";
	char str[1024];

	if(!FCA_VAR_IS_ON(FCA_log))
		return;
#ifdef CGI
	if(FCA_CGI_mode)
		snprintf(strIP, 31, "(%s) ", getenv("REMOTE_ADDR"));
#endif
	va_start(argptr,msg);
	vsnprintf(strtxt, 1004, msg, argptr);
	va_end(argptr);

	snprintf(str, 1023, "%s%s", strIP, strtxt);

	SU_WriteToLogFile(FCA_logf, str);
}

void FCA_upd_debuglevel()
{
	FFSS_PrintDebug(1, "(client) debug level has changed to level %d\n", (int)(FCA_debuglevel[0]-'0'));
	N_DebugLevel=(int)(FCA_debuglevel[0]-'0');
}

void FCA_upd_skin()
{
	int i=0;
	
		/* look for the skin in the list */
	while( FCA_SKINS[i].name && !(!strcmp(FCA_SKINS[i].name,FCA_skin_name) && (!FCA_CGI_mode || FCA_SKINS[i].canCGI)))
		i++;
	if(!FCA_SKINS[i].name) {
		FCA_print_cmd_err("cannot load this skin, restoring the default skin");
		FCA_skin=(FCA_Pskin)&(FCA_SKINS[0]);
		sprintf(FCA_skin_name, FCA_SKINS[0].name);
	} else {
		FCA_skin=(FCA_Pskin)&(FCA_SKINS[i]);
		FFSS_PrintDebug(1, "(client) the skin has changed to '%s'\n", FCA_skin_name);
	}
}

void FCA_upd_log()
{
	if(FCA_logf) {
		if(FCA_VAR_IS_ON(FCA_log))
			FCA_printlog("logging stopped");
		SU_CloseLogFile(FCA_logf);
	}
	if(FCA_VAR_IS_ON(FCA_log)) {
		FCA_logf=SU_OpenLogFile(FCA_logfile);
		if(!FCA_logf) {
			FCA_print_cmd_err("cannot open log file, disabling log");
			sprintf(FCA_log, "off");
		} else {
			FFSS_PrintDebug(5, "(client) logfile opened\n");
			FCA_printlog("starting to log");
		}
	}		
	
}

void FCA_upd_logfile()
{
	if(FCA_VAR_IS_ON(FCA_log)) {
		SU_CloseLogFile(FCA_logf);
		FCA_logf=SU_OpenLogFile(FCA_logfile);
		if(!FCA_logf) {
			FCA_print_cmd_err("cannot open log file, disabling log");
			sprintf(FCA_log, "off");
		} else {
			FFSS_PrintDebug(5, "(client) logfile reopen\n");
			FCA_printlog("restarting to log");
		}
	}
}
