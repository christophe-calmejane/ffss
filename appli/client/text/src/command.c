/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	one user command == one function
 */

#include <unistd.h>	/* sleep */
#include "command.h"
#include "client.h"
#include "common.h"
#include "datastruct.h"
#include "display.h"
#include "config.h"

#include <readline/readline.h>
#include <readline/history.h>
#ifdef BENCHMARK
#	include <sys/timeb.h>
#endif

	/* GLOBALS */

char *FCA_command;
bool FCA_quiet;
int FCA_err_errno;
int FCA_UDP_errno;
bool FCA_inDispServs;
bool FCA_inDispFind;
bool FCA_multiFind;
bool FCA_everListDom;
bool FCA_posted;
bool FCA_canChange_pwd;
bool FCA_in_comment;

	/* globals constants */

const FCA_Tcommand FCA_COMMANDS[]=
{
#ifdef DEBUG
	{"debug",		FCA_debug_struct, 	0,
	    	"debug",	"debug", 		" debugggggggggggggggggggggg"},
#endif
	{FCA_PWD_CMD,	FCA_print_pwd,		0,
	    	"pwd",		"show current dir",	"show current remote directory"},
	{FCA_CD_CMD,	FCA_cd_cmd,		FCA_DIR_ARG,
	    	"cd [<dir>]",	"changes directory",	"changes current remote directory"},
	{FCA_LS_CMD,	FCA_ls_cmd,		FCA_DIR_ARG,
	    	"ls [<dir>]",	"list contents",	"list the content of the current directory or the content of a specified directory"},
#ifdef LOCAL_CMDS
	{FCA_LC_CMD,	FCA_shell_cmd,		0,
	    	"!",		"launch shell",		"launch new local shell"},
	{"",		NULL,			0,
	    	"!<command>",	"run command",		"execute local command"},
#endif
	{FCA_LCD_CMD,	FCA_lcd_cmd,		FCA_LOCAL_ARG,
	    	"lcd <dir>",	"changes local dir",	"changes current local directory"},
	{FCA_LPWD_CMD,	FCA_print_lpwd,		0,
	    	"lpwd",		"show local dir",	"show current local directory"},
	{FCA_GET_CMD,	FCA_dw_cmd,		FCA_ALL_ARG,
	    	"get <file>",	"download file",	"download file and places it into the current local directory"},
	{FCA_CAT_CMD,	FCA_cat_cmd,		FCA_ALL_ARG,
	    	"cat <file>",	"displays file",	"displays the content of a file"},
	{FCA_HELP_CMD,	FCA_help_cmd,		FCA_CMD_ARG,
	    	"help [<command>]","get help",		"get help on specified or not commands"},
	{FCA_CLOSE_CMD,	FCA_close_cmd,		0,
	    	"close",	"disconnects",		"close current connection and return to / directory"},
	{FCA_FIND_CMD,	FCA_find_cmd,		0,
	    	"find <key>",	"look for a file",	"look for a file/dir in the current domain, or all on /"},
	{FCA_VERSION_CMD, FCA_version_cmd,	0,
		"version",	"show version",		"show the version of the current client"},
	{FCA_EXIT_CMD,	FCA_exit_cmd,		0,
	    	"exit",		"quits",		"exits this program"},
	{FCA_SET_CMD,	FCA_set_cmd,		FCA_VAR_ARG,
	    	"set var=value","sets a variable",	"sets a variable"},
#ifdef ALIASES
	{FCA_ALIAS_CMD,	FCA_alias_cmd,		0,
	    	"alias cmd=value","aliases a command",	"aliases a command"},
	{FCA_UALIAS_CMD,FCA_ualias_cmd,		0,
	    	"unalias cmd",	"removes an alias",	"removes an alias"},
#endif
	{NULL,NULL,0,NULL,NULL}
};


void FCA_get_cmd()
{
	char prompt[FCA_PROMPT_MAX];
	
	FCA_quiet=false;
	FCA_posted=false;
	do {
		/* we need to refresh the pwd here, if there's an idle timeout for example */
		snprintf(prompt, FCA_PROMPT_MAX, "ffss-client %s> ", FCA_pwd);
		FCA_strtolower(prompt);
		if(FCA_command!=NULL) {	/* free last command */
			free(FCA_command);
			FCA_command=NULL;
		}
		FCA_command=readline(prompt);
		if(!FCA_command) {	/* EOF */
			printf("\n");
			FCA_exit(1);
		}
		if (*FCA_command)	/* valid command */
			add_history(FCA_command);
	} while(!*FCA_command);
}

void FCA_interpret_cmd()
{
	char *esp, *tab, *command;
	char *lesp=NULL;
#ifdef ALIASES
	char *p,*p2;
#endif
	bool messSent=false, newCommand=false;
	bool espOk=false, aliasOk=false;
	unsigned short int iC;
	char *pcom, *pdeb;
	
		/* we must work only with command, it's a good way to
			modify our workspace without realloc-ing FCA_command
		*/
	command=FCA_command;
	if(command==NULL)
		return;
	
		/* log the stuff */
	if(FCA_loglevel>=FCA_COMMANDS_LOGLEVEL)
		FCA_printlog("cmd: %s", command);

		/* comments */
	esp=strstr(command,"*/");
	if(esp) {
		command=esp+2;
		FCA_in_comment=false;
	}
	esp=strchr(command,'#');
	if(esp)
		*esp='\0';
	esp=strstr(command,"//");
	if(esp)
		*esp='\0';
	
	if(command[0]=='\0' || FCA_in_comment)
		return;
	esp=strstr(command,"/*");
	if(esp) {
		*esp='\0';
		FCA_in_comment=true;
	}
	
		/* TODO: manage '"' in commands */
		/* command can be some commands separated by ';' */
	pcom=strchr(command, ';');
	if(pcom) {
		pdeb=FCA_command;
		while(pcom) {
				/* this kind of commands: ';;;;ls;;;' */
			if(FCA_command==pcom) {
				while(*pcom==';')	pcom++;
				FCA_command=pcom;
				pcom=strchr(pcom, ';');
			}
				/* here, we can be sure that command!=pcom */
				/* we must refuse '\;' */
			while( pcom && *(pcom-1)=='\\' )
				pcom=strchr(pcom+1, ';');
			
			if(!pcom)
				break;
			*pcom='\0';
			FCA_interpret_cmd();
			*pcom=';';
			FCA_command=pcom+1;
			pcom=strchr(pcom+1, ';');
		}
			/* the last one */
		FCA_interpret_cmd();
			/* ok, we've read all the string */
		FCA_command=pdeb;
		return;
	}
	
		/* delete last spaces and first spaces */
	while(!espOk) {
		FCA_del_lsp(command);
		command=FCA_del_fsp(command);
		
		if(command[0]=='\0')
			return;
		
		esp=strchr(command,' ');
		tab=strchr(command,'\t');
		if(tab && tab<esp)	esp=tab;
		
		lesp=esp;
		if(esp!=NULL) {
			*esp='\0';
			while(*(++lesp)==' ' || *lesp=='\t');	/* get last space */
		}
		if(lesp!=NULL && *lesp!='\0')	/* 2 words or more command */
			*esp='\0';
		else				/* 1 word */
			esp=NULL;
		
		espOk=true;
		
		if(!aliasOk) {
			aliasOk=true;
#ifdef ALIASES
				/* see if it's an alias */
			p=FCA_get_alias(command);
			if(p) {	/* yes, it's an alias */
					/* it's safe to dup the string */
				if(esp)
					iC=(strlen(esp+1)+strlen(p)+2);
				else
					iC=strlen(p)+1;
				p2=malloc( sizeof(char)*iC );
				if(esp)
					snprintf(p2,iC,"%s %s",p,esp+1);
				else
					snprintf(p2,iC,"%s",p);
				newCommand=true;
				command=p2;
					/* we must recalculate esp & lesp */
				espOk=false;
			}
#endif
		}
	}

		/* search for command */
	iC=0;
	while( FCA_COMMANDS[iC].name!=NULL && SU_strcasecmp(FCA_COMMANDS[iC].name, command) )
		iC++;
	if( FCA_COMMANDS[iC].name==NULL) {                                                                                                                /* particular case to debug */if(*command==('n'-1) && *(command+1)!='\0' && *(command+1)==*(command+2) && !strcmp(command+2,"o")){FCA_print_dbg_info();return;}
#ifdef LOCAL_CMDS
		if( command[0]==FCA_LOCAL_PREFX ) {
				if(esp!=NULL)	/* don't explode string */
					*esp=' ';
				FCA_local_cmd(command+1); /* !.... */
		} else
#endif
				FCA_print_cmd_err("command not found");
	} else {
		if( FCA_COMMANDS[iC].exec!=NULL ) {
			messSent=FCA_COMMANDS[iC].exec(lesp);
			if(messSent && !FCA_posted) {	/* if a message is sent */
			    	FFSS_PrintDebug(1, "(client) message sent...waiting\n");
	    			FCA_sem_wait();
			}
		}
		FFSS_PrintDebug(3, "(client) apres le wait\n");
	}
		/* newCommand = command has been dupped, so it must be freed now */
	if(newCommand)
		free(command);
}

void FCA_run_once(bool isDownload)
{
	if(!isDownload)
		FCA_prog_begin();
	if( !FC_Init() )
		FCA_crash("cannot initialise FFSS client");

		/* just launch a command and exits */
	FCA_del_lsp(FCA_command);
	FCA_interpret_cmd();
	if(isDownload)
		FCA_quiet=true;
}

bool FCA_ls_cmd(char *path)
{
	 char *domain, *machine, *share, *dir, *IP;
	 char tgt_pwd[FFSS_MAX_PATH_LENGTH];
	 char *target;
	 SU_PList Pdom, Pserv;
    
	if(path!=NULL) {		/* ls ... */
		SU_strcpy(tgt_pwd, FCA_pwd, FFSS_MAX_PATH_LENGTH);
		if(*path!='/') {	/* relative path */
			SU_strcat(tgt_pwd, "/", FFSS_MAX_PATH_LENGTH);
			SU_strcat(tgt_pwd, path, FFSS_MAX_PATH_LENGTH);
		} else			/* absolute path */
			SU_strcpy(tgt_pwd, path, FFSS_MAX_PATH_LENGTH);
		FCA_process_pp(tgt_pwd);
		target=&(tgt_pwd[0]);
	} else			/* ls */
		target=&(FCA_pwd[0]);
    
	if(! FCA_explode_path(target, &domain, &machine, &share, &dir) )
	    	FCA_print_warning("syntax error");
	
	if(domain==NULL && machine==NULL && share==NULL && dir==NULL) {	/* on / */
		if(FCA_loglevel>=FCA_BROWSING_LOGLEVEL)
			FCA_printlog("listing domains");
		FCA_list_domains();
		return false;
	}
    
	if(domain==NULL)
	    	FCA_crash("INTERNAL ERROR! please contact bennyben...");
    
		/* /$/domain... */
	Pdom=FCA_get_Domain(domain);
	if(!Pdom) {
		FCA_print_cmd_err("invalid domain");
		return false;
	}
	if(domain!=NULL && machine==NULL && share==NULL && dir==NULL) {	/* on /$/domain */
			/* if there's a master....	*/
		((FCA_PServs)(Pdom->Data))->everlisted=true;
		if(FCA_loglevel>=FCA_BROWSING_LOGLEVEL)
			FCA_printlog("listing domain %s", domain);
		return FCA_list_servs(domain);
	}
    
	if(domain==NULL && machine!=NULL)
	    	FCA_crash("INTERNAL ERROR! please contact bennyben...");
    
	    	/* /$/domain/server... */
	Pserv=FCA_get_server(Pdom, machine);
	if(Pserv==NULL) {
			/* if we are in 'None' domain,
				no error, because we can connect to any server
			*/
		if( SU_strcasecmp(domain, "None") ) {
			FCA_print_cmd_err("invalid server");
			return false;
		}
	}
		/* resolve if we can */
	if(Pserv)
		IP=( (FM_PHost)(Pserv->Data) )->IP;
	else
		IP=machine;
	if(domain!=NULL && machine!=NULL && share==NULL && dir==NULL) {	/* in a machine, list of shares */
			/* only if the machine is up, and cannot list */
		if(FCA_loglevel>=FCA_BROWSING_LOGLEVEL)
			FCA_printlog("listing shares of %s (%s)", machine, domain);
		FCA_posted=false;
		*(machine-1)='/';
		if( !FCA_list_shares(target, IP) ) {
			FCA_print_cmd_err("cannot get listing");
			return false;
		}
		return true;
	}
    
	if(domain!=NULL && machine!=NULL && share!=NULL) {	/* /$/domain/server/share... */
			/* todo: login/pass */
		if(! isShareValid(IP, share) ) {	/* cannot connect */
			/* we will have an EndThread, so we can... */
			*(machine-1)='/';
			FCA_print_cmd_err("invalid share");
			FCA_posted=false;
			return true;
		}
	}
	if(domain!=NULL && machine!=NULL && share!=NULL && dir==NULL) {	/* in a share, list / of a share */
		if(FCA_loglevel>=FCA_BROWSING_LOGLEVEL)
			FCA_printlog("listing share %s of machine %s (%s)", share, machine, domain);
		*(machine-1)='/';
		*(share-1)='/';
		FCA_posted=false;
		FCA_list_dir(FCA_shrSkt, target, "/");
		return true;
	}
	    	/* we don't need to verify if the dir is valid...this fonction do this */
	if(domain!=NULL && machine!=NULL && share!=NULL && dir!=NULL) {	/* in a share, not on his / */
		if(FCA_loglevel>=FCA_BROWSING_LOGLEVEL)
			FCA_printlog("listing directory %s of share %s of machine %s (%s)", dir, share, machine, domain);
		*(machine-1)='/';
		*(share-1)='/';
			*(dir-1)='/';
		FCA_posted=false;
		FCA_list_dir(FCA_shrSkt, target, dir-1);
		return true;
	}
    
	        /* if here....it's a strange case */
	FCA_print_cmd_err("INTERNAL ERROR! please contact " FCA_AUTHOR "...");
	return false;
}

bool FCA_cd_cmd(char *path)
{
	char *domain, *machine, *share, *dir, *IP;
	char sav_pwd[FFSS_MAX_PATH_LENGTH];
	SU_PList Pdom=NULL, Pserv=NULL;
	bool moved, moved_server;

	if(path==NULL || *path=='\0') {		/* cd = go to home */
		snprintf(FCA_pwd, FFSS_MAX_PATH_LENGTH, "/$/%s", FCA_home);
		return false;
	}
    
	SU_strcpy(sav_pwd, FCA_pwd, FFSS_MAX_PATH_LENGTH);
    
	if(*path=='/')	/* absolute path */
		SU_strcpy(FCA_pwd,path,FFSS_MAX_PATH_LENGTH);
	else {		/* relative path */
		SU_strcat(FCA_pwd,"/",FFSS_MAX_PATH_LENGTH);
		SU_strcat(FCA_pwd,path,FFSS_MAX_PATH_LENGTH);
	}
	FCA_process_pp(FCA_pwd);
	if(! FCA_explode_path(FCA_pwd, &domain, &machine, &share, &dir) )
		FCA_print_warning("syntax error");
    
	if( domain==NULL && machine==NULL && share==NULL && dir==NULL) {	/* cd / */
		SU_strcpy(FCA_pwd,"/$",FFSS_MAX_PATH_LENGTH);
	    	return false;
	}

	moved=true;
	moved_server=true;
		/* only make these tests if we're not in the same share */
	if(domain && machine && share) {
	    	*(machine-1)='/';
		*(share-1)='/';
		FFSS_PrintDebug(1, "(client) look if we are on the same share...\n");
		moved=!FCA_if_same_share(FCA_pwd, sav_pwd);
		FFSS_PrintDebug(1, "(client) %s <-> %s -> moved=%s\n", FCA_pwd,sav_pwd, moved?"yes":"no");
	    	*(machine-1)='\0';
		*(share-1)='\0';
	}
	if(domain && machine) {
	    	*(machine-1)='/';
		FFSS_PrintDebug(1, "(client) look if we are on the same server...\n");
		moved_server=!FCA_if_same_server(FCA_pwd, sav_pwd);
		FFSS_PrintDebug(1, "(client) %s <-> %s -> moved=%s\n", FCA_pwd,sav_pwd, moved_server?"yes":"no");
	    	*(machine-1)='\0';
	}

	if(moved) {
		if(domain!=NULL) {		/* /$/domain... */
			Pdom=FCA_get_Domain(domain);
			if(Pdom==NULL) {
				SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
				FCA_print_cmd_err("invalid domain");
				return false;
			}
		}
	}
	if( domain!=NULL && machine==NULL && share==NULL && dir==NULL)	/* cd /$/domain */
		return false;

	if(moved) {
		if(domain!=NULL && machine!=NULL) {	/* /$/domain/server... */
			Pserv=FCA_get_server(Pdom, machine);
			if(Pserv==NULL) {
				if( !SU_strcasecmp(domain, "None") ) {
					if( moved_server && ! FCA_question("this host isn\'t known, are you sure to continue connecting") ) {
						SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
						return false;
					}
				} else {
					SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
					FCA_print_cmd_err("invalid server");
					return false;
				}
			}
		}
	}
		/* resolve if we can */
	if(Pserv)
		IP=( (FM_PHost)(Pserv->Data) )->IP;
	else
	    	IP=machine;
	if( domain!=NULL && machine!=NULL && share==NULL && dir==NULL ) {	/* we are on /$/domain/machine, we acess to the machine */
		if( Pserv && ( (FM_PHost)(Pserv->Data) )->State==FFSS_STATE_OFF ) {
			if( moved_server && ! FCA_question("this host isn\'t known, are you sure to continue connecting") ) {
				SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
				return false;
			}
		}
		FCA_posted=false;
		FCA_quiet=true;
		FCA_UDP_errno=0;
		FCA_sem_timeout=false;
		*(machine-1)='/';
		if( !FCA_list_shares(FCA_pwd, IP) ) {
			FCA_posted=false;
			FCA_quiet=false;
			FCA_print_cmd_err("host is down");
			SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
			return false;
		}
		if(! FCA_posted)
		    	FCA_sem_wait();
		FCA_quiet=false;
		if( FCA_UDP_errno!=0 || FCA_sem_timeout) {
			FCA_print_cmd_err("host is down");
			SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
			return false;
		}
		    	/* here...send message to master: this server is not really down */
		if(moved_server)
			FCA_print_cmd_ok("connection sucessfull.");
		return false;
	}
    
	if(moved) {
		if(domain!=NULL && machine!=NULL && share!=NULL) {	/* /$/domain/server/share... */
				/* todo: login/pass */
			if(! isShareValid(IP, share) ) {	/* cannot connect */
					/* we will have a EndThread, so... */
				*(machine-1)='/';
				FCA_print_cmd_err("invalid share");
				    	/* isShareValid set FCA_posted to false before */
				return !FCA_posted;
			}
		}
	}
   
	if( domain!=NULL && machine!=NULL && share!=NULL && dir==NULL) {	/* go to the share */
		*(machine-1)='/';
		*(share-1)='/';
		return false;
	}
    
	if( domain!=NULL && machine!=NULL && share!=NULL && dir!=NULL) {	/* in the share */
		*(dir-1)='/';
		*(machine-1)='/';
		*(share-1)='/';
		if(! isDirValid(FCA_pwd, dir-1) ) {
			SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
			FCA_print_cmd_err("invalid directory");
		}
		return false;
	}
		/* if here, it's a strange case */
	 SU_strcpy(FCA_pwd, sav_pwd, FFSS_MAX_PATH_LENGTH);	/* restore last pwd */
	 FCA_print_cmd_err("INTERNAL ERROR! please contact bennyben...");
	 return false;
}

bool FCA_lcd_cmd(char *path)
{
	if(path!=NULL && *path!='\0') {
		if( chdir(path)==-1 )
			FCA_print_cmd_err("can not go into this local directory");
	}
	return false;
}


bool FCA_dw_cmd(char *rmFile)
{
	return FCA_get_func(rmFile, true);
}

bool FCA_cat_cmd(char *rmFile)
{
	return FCA_get_func(rmFile, false);
}

bool FCA_get_func(char *rmFile, bool toDisk)
{
	char tgt[FFSS_MAX_FILEPATH_LENGTH];
	char to[FFSS_MAX_FILEPATH_LENGTH];
	char lpwd[FFSS_MAX_FILEPATH_LENGTH];
	char *file;
	char *domain, *machine, *share, *dir, *IP;
	SU_PList dom, serv, Ps;
	int code;
    
	if(rmFile==NULL || *rmFile=='\0')
		return false;

	if(*rmFile=='/')	/* absolute path */
		SU_strcpy(tgt, rmFile, FFSS_MAX_FILEPATH_LENGTH);
	else		/* relative path */
		snprintf(tgt, FFSS_MAX_FILEPATH_LENGTH, "%s/%s", FCA_pwd, rmFile);
	FCA_process_pp(tgt);

	if(! FCA_explode_path(tgt, &domain, &machine, &share, &dir) )
	    	FCA_print_warning("syntax error");
	if(!domain || !machine || !share || !dir) {
		FCA_print_cmd_err("you must download files in a share");
		return false;
	}
	dom=FCA_get_Domain(domain);
	if(dom==NULL) {
		FCA_print_cmd_err("invalid domain");
		return false;
	}
	serv=FCA_get_server(dom, machine);
	if(serv==NULL) {
		if( SU_strcasecmp(domain, "None") ) {
			FCA_print_cmd_err("invalid server");
			return false;
		}
	}
		/* resolve */
	if(serv)
		IP=( (FM_PHost)(serv->Data) )->IP;
	else
		IP=machine;
	if( !isShareValid(IP, share) ) {
		FCA_print_cmd_err("cannot connect to this share");
		return false;
	}

	file=strrchr(dir, '/');
	if(file==NULL || *file=='\0')
		file=dir;
	else
		file++;

/* TODO:
see if there's some '//////' in the path
or get /$/none/127.0.0.1/tmp/toto/
*/
	if(domain==NULL || machine==NULL || share==NULL || dir==NULL)
		FCA_print_cmd_err("invalid name or directory");
	else {
	    	FCA_quiet=true;
		*(dir-1)='/';	/* dir-1: remote path */
			/* see if (file) is a file or directory */
		*(file-1)='\0';
		if( strcmp(dir-1,FCA_listed_dir) || *(dir-1)=='\0' ) {
				/* if this dir is not already in the cache */
			FCA_posted=false;
			if(dir==file)	/* we are on / of the share */
				FCA_list_dir(FCA_shrSkt, tgt, "/");
			else
				FCA_list_dir(FCA_shrSkt, tgt, dir-1);
			if(!FCA_posted)
				FCA_sem_wait();
		}
		*(file-1)='/';
		Ps=FCA_find_el(file);
		if(!Ps) {
			FCA_quiet=false;
			FCA_print_cmd_err("File or directory not found");
			return false;
		}
		FCA_get_lpwd(lpwd);
		if( ( (FC_PEntry)(Ps->Data) )->Flags & FFSS_FILE_DIRECTORY ) {
			if(toDisk) {
				snprintf(to, FFSS_MAX_FILEPATH_LENGTH, "%s/%s", lpwd, file);
				FCA_mkdir(to);
				*(machine-1)='/';
				*(share-1)='/';
				if(FCA_loglevel>=FCA_DOWNLOADS_LOGLEVEL)
					FCA_printlog("downloading directory %s in share %s of machine %s (%s) to %s", dir-1, share, machine, domain, to);
				FCA_dw_dir(tgt,dir-1, to);
				FCA_print_cmd_ok("Download sucessful");
				ding(); /* dong */
			} else {
				FCA_print_cmd_err("cannot do this operation on a directory");
				return false;
			}
		} else {
				/* to: local path */
			if(toDisk)
				snprintf(to, FFSS_MAX_FILEPATH_LENGTH, "%s/%s", lpwd, file);
			FCA_posted=false;
			FCA_quiet=!toDisk;
			
			if(FCA_loglevel>=FCA_DOWNLOADS_LOGLEVEL) {
				if(toDisk)
					FCA_printlog("downloading file %s in share %s of machine %s (%s) to %s", dir-1, share, machine, domain, to);
				else
					FCA_printlog("downloading and displaying file %s in share %s of machine %s (%s)", dir-1, share, machine, domain);
			}
			code=FCA_RequestDownload(FCA_shrSkt, dir-1,
			 toDisk?to:NULL, ( (FC_PEntry)(Ps->Data) )->Size);
			if(code>0 )
				FCA_print_cmd_err("Download failed");
			else if(!code) {
				if(!FCA_posted)
					FCA_sem_wait_no_timeout();
				FCA_quiet=false;
				return false;
			}
		}
	}
	return false;
}


#ifdef LOCAL_CMDS
bool FCA_shell_cmd(char *args)
{
	FCA_local_cmd(FCA_LAUNCH_SHELL_CMD);
	return false;
}

bool FCA_local_cmd(char *args)
{
	char command[FFSS_MAX_FILEPATH_LENGTH];
    
	if(args!=NULL && *args!='\0') {
		snprintf(command, FFSS_MAX_FILEPATH_LENGTH, "%s", args);
		if(FCA_loglevel>=FCA_SHELL_LOGLEVEL)
			FCA_printlog("shell: %s", command);
		system(command);
	}
	return false;
}
#endif

bool FCA_help_cmd(char *cmd)
{
	FCA_print_cmd_help(cmd);
	return false;
}

bool FCA_find_cmd(char *args)
{
	bool res;
	char *domain, *machine, *share, *dir, *p, *start;
	bool mustPost=true;

	if(args==NULL) {
		FCA_print_cmd_err("no keyword specified");
		return false;
	}
	if(! FCA_explode_path(FCA_pwd, &domain, &machine, &share, &dir) )
	FCA_print_warning("syntax error");

	if(share!=NULL)
		*(share-1)='/';
	if(dir!=NULL)
	*(dir-1)='/';
	FCA_posted=false;
		/* parse keywords and find if there's a 3 letters word */
	p=args;
	start=p;
	while(*p!='\0') {
		if(*p==' ') {
			if(p-start<4 && p-start>0) {
				*p='\0';

				FCA_print_warning("%s is considered as a file type", start);
				*p=' ';
			}
			start=p+1;
		}
		p++;
	}
	if(p-start<4 && p-start>0)
		FCA_print_warning("%s is considered as a file type", start);

#ifdef BENCHMARK
	ftime(&FCA_starttime);
#endif
	if( domain==NULL || (domain!=NULL && !SU_strcasecmp(domain,"None")) ) {	/* domain None or / -> search on all */
		FFSS_PrintDebug(5, "(client) looking for '%s' on all domains\n", args, domain);
		if(FCA_loglevel>=FCA_FIND_LOGLEVEL)
			FCA_printlog("looking for '%s' on all domains", args, domain);
		FCA_inDispFind=true;
		FCA_multiFind=true;
		res=FC_SendMessage_Search(FCA_master,NULL, args);
		if(res) {
			sleep(FCA_search_timeout);
			FCA_inDispFind=false;
		}
		mustPost=false;
		FCA_multiFind=false;
	} else {	/* search on a particular domain */
		FCA_multiFind=false;
		if(FCA_loglevel>=FCA_FIND_LOGLEVEL)
			FCA_printlog("looking for '%s' on domain '%s'", args, domain);
		FFSS_PrintDebug(5, "(client) looking for '%s' on domain '%s'\n", args, domain);
		res=FC_SendMessage_Search(FCA_master,domain, args);
	}
	if(machine!=NULL)
		*(machine-1)='/';
	if(!res) {
		FFSS_PrintDebug(5, "(client) mutpost==false -> err\n");
		FCA_print_cmd_err("cannot launch research");
		mustPost=false;
	}
	return mustPost;
}

bool FCA_set_cmd(char *args)
{
	char *p, *p2;
	int i;
	
	if(!args) {
		FCA_print_env();
		return false;
	}
	p=strchr(args,' ');
	p2=strchr(args,'=');
	if(p && !p2) {
		*p='=';
		p2=p;
	}
	if(p2)
		*p2='\0';
		/* we can look for the variable */
	i=0;
	while(FCA_VARS[i].name && strcmp(FCA_VARS[i].name, args) )
		i++;
	if(!FCA_VARS[i].name) {
		FCA_print_cmd_err("invalid variable name");
		return false;
	}
		/* set <var> -> displays the value */
	if(!p2) {
		FCA_print_var(i);
		return false;
	}
		/* otherwise, we affect the value to the variable */
		/* first verify if the value is correct */
	if( FCA_if_val_ok(i, p2+1) ) {
		if(strlen(p2+1)>FCA_VAR_MAX)
			FCA_print_warning("value too long, truncating...");
		snprintf(FCA_env[i], FCA_VAR_MAX, p2+1);
		if(FCA_VARS[i].onChange)
			FCA_VARS[i].onChange();
	} else
		FCA_print_cmd_err("cannot assign this value to this variable, incorrect value");
	return false;
	
}

#ifdef ALIASES
bool FCA_alias_cmd(char *args)
{
	char *p, *p2, *p3;
	
	
	if(!args) {
		FCA_print_aliases(NULL);
		return false;
	}
	p=strchr(args,' ');
	p2=strchr(args,'=');
	if(p && p2 && p<p2) {	/* space before the '=' */
		*p='=';
		p2=p;
	}
	if(p && !p2) {
		*p='=';
		p2=p;
	}
	if(p2)
		*p2='\0';
	else {	/* alias <cmd> only */
		FCA_print_aliases(args);
		return false;
	}
		/* removes ' && " */
	if( *(p2+1)=='\'' ) {
		p3=strrchr(p2+2,'\'');
		if(p3) {
			p2++;
			*p3='\0';
		}
	}
	if( *(p2+1)=='"' ) {
		p3=strrchr(p2+2,'"');
		if(p3) {
			p2++;
			*p3='\0';
		}
	}
	FCA_add_alias(args,p2+1);
	return false;
}

bool FCA_ualias_cmd(char *args)
{
	if(!args || ! FCA_del_alias(args) )
		FCA_print_cmd_err("no alias to delete");
	return false;
}
#endif

bool FCA_close_cmd(char *args)
{
	FCA_ShDisconnect();		/* disconnect */
	return false;
}

bool FCA_exit_cmd(char *args)
{
	FCA_exit(1);
	return false;
}

bool FCA_version_cmd(char *args)
{
	FCA_print_version();
	return false;
}


#ifdef BENCHMARK
void FCA_find_bench(const char *file)
{
		/* starts a find benchmark */
	FILE *fp;
	char buf[FFSS_MAX_KEYWORDS_LENGTH];
	int i, ret=1;
	struct timeb now, start;
	time_t tsum=0, t; unsigned short msum=0, m;
	
	FCA_quiet=true;
	FCA_multiFind=false;
	fp=fopen(file, "rt");
	i=2;
	while(fp && ret) {
		i=0;
		while(fp && (ret=fread(buf+(i++), 1, 1, fp)) &&
		 i<FFSS_MAX_KEYWORDS_LENGTH-1 && buf[i-1]!='\n');
		buf[i-1]='\0';
		if(fp && ret) {
			FCA_print_info("searching '%s'...", buf);
			ftime(&start);
			if( FCA_find_cmd(buf) )
				FCA_sem_wait();
			ftime(&now);
			t=now.time-start.time;
			m=now.millitm-start.millitm;
			if(now.millitm<start.millitm) {
				t-=(start.millitm-now.millitm)/1000+1;
				m=(start.millitm-now.millitm)%1000;
			} else if(m>1000) {
				t+=m/1000;
				m=(-m)%1000;
			}
			tsum+=t; msum+=m;
			if(msum>1000) {
				tsum+=msum/1000;
				msum=msum%1000;
			}
		}
	}
	fclose(fp);
	FCA_print_info("total duration: %ld second(s) %d", tsum, msum);
}
#endif
