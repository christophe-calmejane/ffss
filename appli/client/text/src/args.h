/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	command-line arguments management
 */

#ifndef _FCA_ARGS_H_
#define _FCA_ARGS_H_

#include <ffss.h>

    /* shorts args */
#define FCA_SHARG_GET_SHARES		'L'
#define FCA_SHARG_LS_DIR		'l'
#define FCA_SHARG_EXEC_CMD		'e'
#define FCA_SHARG_GET_VERSION		'v'
#define FCA_SHARG_GET_HELP		'h'
#define FCA_SHARG_CFG_FILE		'c'
#define FCA_SHARG_DBG_LEVEL		'd'
#define FCA_NO_ANSI_CFG			'a'
#define FCA_SHARG_MASTER		'm'
#define FCA_SHARG_SKIN			's'
    /* long args */
#define FCA_LGARG_GET_VERSION		"version"
#define FCA_LGARG_GET_HELP		"help"

struct FCA_Targs
{
	int argc;
	char **argv;
	unsigned int mode;
	bool can_ansi;
	char *domain;
	char *machine;	/* the target machine */
	char *share;	/* the share name */
	char *machToSh;	/* machine to get shares */
	char *dirToLs;	/* a directory to explore */
	char *cfg_file;	/* the config file */
	char *cmd;		/* the command to execute */
	char *dbg_level;	/* the debug level */
	char *master;
	char *skin;
} FCA_args;

void FCA_get_args(void);
unsigned short int FCA_interpret_args(void);
unsigned short int FCA_look_if_long_arg(char *parg);

#endif
