/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	command-line arguments management
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
#include "args.h"
#include "display.h"
#include "common.h"
#include "client.h"

void FCA_get_args(void)
{
	int invalid_arg;

	    	/* more than 65535 args! (just for bad hackers) */
	if( FCA_args.argc>65535 ) {
		printf("more than 65535 arguments...\nAre you trying to make this program crashing ?\n");
		FCA_exit(-1);
	}
		/* verify that all args are valid.
		    Otherwise prints an error */
	invalid_arg=FCA_interpret_args();
	if(invalid_arg)
	        FCA_print_arg_err(invalid_arg);
}

unsigned short int FCA_interpret_args(void)
{
		/* return arg number in case of a bad argument */
	int iarg;
	char **nextToStore = NULL;
	unsigned short int invalid_arg = 0;
	char *parg;
	unsigned short int stop;

	FCA_args.can_ansi=true;
	iarg=1;
	while( iarg<FCA_args.argc && !invalid_arg ) {
		parg=FCA_args.argv[iarg];
		if( *parg=='-') {		/* -... */
			stop=0;
			if( !FCA_look_if_long_arg(parg) ) {
				parg++;
				while( *parg!='\0' && !stop ) {	/* each letter after the - */
					switch( *parg ) {
					case FCA_SHARG_GET_SHARES:	/* get shares */
					    	nextToStore=&(FCA_args.machToSh);
						break;
					case FCA_SHARG_LS_DIR:		/* list a shared dir */
						nextToStore=&(FCA_args.dirToLs);
						break;
					case FCA_SHARG_CFG_FILE:	/* specify a config file */
						nextToStore=&(FCA_args.cfg_file);
						break;
					case FCA_SHARG_DBG_LEVEL:	/* sets debug level */
						nextToStore=&(FCA_args.dbg_level);
						break;
					case FCA_NO_ANSI_CFG:		/* disable ansi */
						FCA_args.can_ansi=false;
						break;
					case FCA_SHARG_MASTER:		/* sets the master */
						nextToStore=&(FCA_args.master);
						break;
					case FCA_SHARG_SKIN:		/* sets the skin */
						nextToStore=&(FCA_args.skin);
						break;
					case FCA_SHARG_EXEC_CMD:	/* exec a command */
						nextToStore=&(FCA_args.cmd);
						break;
#ifdef BENCHMARK
					case FCA_SHARG_BENCHFILE:	/* run a benchmark from file */
						nextToStore=&(FCA_args.benchfile);
						break;
#endif
					case FCA_SHARG_GET_VERSION:	/* get version */
						FCA_version();
						break;
					case FCA_SHARG_GET_HELP:	/* get help */
						FCA_print_help();
						break;
					default:
						if( nextToStore==NULL) {
							invalid_arg=iarg;
							stop=1;
						} else {	/* for server's names like '-u', use '--u' */
							*nextToStore=parg;
							nextToStore=NULL;
							stop=1;
						}
						break;
					}
					parg++;
				}
			}
		}  else {
				/* ... */
			if( nextToStore==NULL)
	        		invalid_arg=iarg;
			else {
		    		*nextToStore=FCA_args.argv[iarg];
		    		nextToStore=NULL;
			}
		}
		iarg++;
	}
	return invalid_arg;
}

unsigned short int FCA_look_if_long_arg(char *parg)
{
		/* return if this arg can be a long arg */
	if( *parg=='-' && *(parg+1)=='-') {	/* --... */
		parg+=2;
		if( !strcmp(parg,FCA_LGARG_GET_VERSION) ) {    /* get version */
			FCA_version();
			return 1;
		} else {
			if( !strcmp(parg,FCA_LGARG_GET_HELP) ) {  /* get help */
				FCA_print_help();
				return 1;
			}
		}
	}
	return 0;	/* not --... : invalid */
}
