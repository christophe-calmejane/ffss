/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	common functions
 */
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

#ifndef _FCA_COMMON_H_
#define _FCA_COMMON_H_

#include "display.h"

#include <ffss.h>
#include <readline/readline.h>
#include <skyutils.h>

#define FCA_VARNAME_MAX		20
#define FCA_ALIAS_MAX		20

#ifdef _WIN32	/* win */
#	include <direct.h>
#	define FCA_mkdir(x)	mkdir(x)
#else		/* unix */
#	include <sys/stat.h>
#	define FCA_mkdir(x)	mkdir(x,493)    /* rwx r-x r-x -> (755)octal -> (493)decimal */
#endif

#include <readline/readline.h>
	/* for different readline versions */
#ifndef rl_completion_matches
#	define rl_completion_matches	completion_matches
#endif
#ifndef ding
#	define FCA_ding ding
#else
#	define FCA_ding rl_ding
#endif

#define FCA_VAR_IS_ON(x)	x[1]=='n'
	/* environment variables
	    to update each time you add a variable
	 */
#define FCA_can_ansi		FCA_env[0]
#define FCA_debuglevel		FCA_env[1]
#define FCA_disp_off		FCA_env[2]
#define FCA_disp_quiet		FCA_env[3]
#define FCA_master		FCA_env[4]
#define FCA_prompt		FCA_env[5]
#define FCA_skin_name		FCA_env[6]
#define FCA_broadcast_timeout	atoi(FCA_env[7])
#define FCA_search_timeout	atoi(FCA_env[8])
#define FCA_operation_timeout	atoi(FCA_env[9])
#define FCA_sort		FCA_env[10]
#define FCA_sort_descending	FCA_env[11]
#define FCA_sort_files_by	FCA_env[12]
#define FCA_sort_shares_by	FCA_env[13]
#define FCA_sort_servers_by	FCA_env[14]
#define FCA_log			FCA_env[15]
#define FCA_logfile		FCA_env[16]
#define FCA_loglevel		FCA_env[17][0]
#define FCA_useConnSock		FCA_env[18]
#define FCA_broadcaaddr		FCA_env[19]
	/*
		WARNING !!!
		modify this each time you add a variable
	*/
#define FCA_skin_env_index	20

#define FCA_FIND_LOGLEVEL	'1'
#define FCA_SHELL_LOGLEVEL	'2'
#define FCA_DOWNLOADS_LOGLEVEL	'3'
#define FCA_BROWSING_LOGLEVEL	'4'
#define FCA_ARGUMENTS_LOGLEVEL	'5'
#define FCA_CGI_LOGLEVEL	'6'
#define FCA_COMMANDS_LOGLEVEL	'7'
#define FCA_ALL_LOGLEVEL	'8'


typedef struct
{
	char *name;
	bool (*exec)(char *args);
	unsigned int arg_type;	/* each command have up to one argument */
	char *syntax;
	char *miniHelp;
	char *help;
} FCA_Tcommand, *FCA_Pcommand;

typedef struct
{
	char *name;
	char *descr;
	char *values;
	void (*onChange)();
} FCA_Tvar, *FCA_Pvar;


	/* global variables */
extern int FCA_compl_wanted;
extern FFSS_PTransfer FCA_Ptrans;
extern bool FCA_sem_timeout;
extern FILE *FCA_logf;


int FCA_RequestDownload(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[], FFSS_LongField size);
unsigned short int FCA_ShConnect(char *IP, char *share, char *login, char *passwd);
void FCA_close_connection();
void FCA_ShDisconnect();
void FCA_exit(int code);

bool isShareValid(char *IP, char *share);
bool isDirValid(char *path, char *dir);

bool FCA_if_same_share(char *path, char *path2);
bool FCA_if_same_server(char *path, char *path2);

	/* thread manipulation */
void FCA_sem_init();
void FCA_sem_wait_no_timeout();
void FCA_sem_wait();
#ifdef _WIN32
void FCA_sem_timer();
#else
void *FCA_sem_timer();
#endif
void FCA_timer_handusr1();
void FCA_sem_post();

void FCA_ignore_usr1();
void FCA_restore_usr1();

char **FCA_completion(char *text, int start, int end);
char *FCA_cmd_gen(char *text, int state);
char *FCA_path_gen(char *text, int state);
char *FCA_var_gen(char *text, int state);
char *FCA_val_gen(char *text, int state);

char *FCA_new_encoded(char *txt);
void FCA_decode(char *txt);

void FCA_del_lsp(char *cmd);
char *FCA_del_fsp(char *cmd);
bool FCA_explode_path(char *path, char **domain, char **machine, char **share, char **dir);
void FCA_process_pp(char *path);
char *FCA_strtoupper(char *str);
char *FCA_strtolower(char *str);

void FCA_dw_dir(char *path, char *dir, char *dest);

unsigned int *FCA_pre_tabsort(int nbEl);
void FCA_sort_chartab(unsigned int *res, const char **els,int Nbels);
SU_PList FCA_sort_hostlist(SU_PList l);
SU_PList FCA_sort_dirlist(SU_PList l);
bool FCA_hostlist_comp(SU_PList p1, SU_PList p2);
bool FCA_dirlist_comp(SU_PList p1, SU_PList p2);
SU_PList FCA_sort_list(SU_PList l, bool (*comp)(SU_PList,SU_PList) );

	/* sendMessage redefinitions */
bool FCA_list_dir(SU_PClientSocket Server,const char Path[], const char Dir[]);
bool FCA_list_shares(const char Path[], const char Server[]);
void FCA_broadcast();
bool FCA_list_servs(const char *domain);
void FCA_list_domains();

void FCA_get_lpwd(char *dir);

bool FCA_if_val_ok(int i, const char *value);

void FCA_printlog(char *msg, ...);

	/* on variables' modification */
void FCA_upd_debuglevel();
void FCA_upd_skin();
void FCA_upd_log();
void FCA_upd_logfile();
void FCA_upd_broadcaddr();

#endif	/* _FCA_COMMON_H_ */
