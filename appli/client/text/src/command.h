/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	one user command == one function
 */

#ifndef _FCA_COMMAND_H_
#define _FCA_COMMAND_H_

#include <skyutils.h>

    /* max */
#define FCA_PROMPT_MAX		1024+16
#define FCA_CMD_MAX		2048

    /* commands */
#define FCA_PWD_CMD		"pwd"

#define FCA_CD_CMD		"cd"

#define FCA_LS_CMD		"ls"

#define FCA_LC_CMD		"!"
#define FCA_LOCAL_PREFX		'!'

#define FCA_LCD_CMD		"lcd"
#define FCA_LPWD_CMD		"lpwd"

#define FCA_GET_CMD		"get"
#define FCA_CAT_CMD		"cat"

#define FCA_HELP_CMD		"help"

#define FCA_CLOSE_CMD		"close"

#define FCA_FIND_CMD		"find"

#define FCA_SET_CMD		"set"

#define FCA_ALIAS_CMD		"alias"
#define FCA_UALIAS_CMD		"unalias"

#define FCA_VERSION_CMD		"version"

#define FCA_EXIT_CMD		"exit"
	/* argument types */
#define FCA_ALL_ARG		1
#define FCA_DIR_ARG		2
#define FCA_LOCAL_ARG		4
#define FCA_CMD_ARG		8
#define FCA_VAR_ARG		16
#define FCA_VAL_ARG		32

	/* extern variables */
extern char *FCA_command;
extern bool FCA_quiet;
extern int FCA_err_errno;
extern int FCA_UDP_errno;
extern bool FCA_inDispServs;
extern bool FCA_inDispFind;
extern bool FCA_multiFind;
extern bool FCA_everListDom;
extern bool FCA_posted;
extern bool FCA_canChange_pwd;
extern bool FCA_in_comment;

void FCA_get_cmd();

void FCA_interpret_cmd();

void FCA_run_once(bool isDownload);

bool FCA_ls_cmd   (char *path	);
bool FCA_cd_cmd	  (char *path	);
bool FCA_lcd_cmd  (char *path	);
bool FCA_dw_cmd   (char *rmFile	);
bool FCA_cat_cmd  (char *rmFile	);
bool FCA_get_func (char *rmFile, bool toDisk);

bool FCA_shell_cmd(char *args	);
bool FCA_local_cmd(char *args	);

bool FCA_help_cmd (char *cmd	);
bool FCA_close_cmd(char *args	);
bool FCA_find_cmd (char *args	);
bool FCA_set_cmd  (char *args	);
bool FCA_exit_cmd (char *args	);
bool FCA_version_cmd(char *args	);

#ifdef ALIASES
bool FCA_alias_cmd(char *args);
bool FCA_ualias_cmd(char *args);
#endif

#ifdef BENCHMARK
void FCA_find_bench(const char *file);
#endif

#endif	/* _FCA_COMMAND_H_ */
