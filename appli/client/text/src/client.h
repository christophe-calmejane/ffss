/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	main functions
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

#ifndef _FCA_CLIENT_H_
#define _FCA_CLIENT_H_

    /* it's ok, there's all in this :) */
#include <ffss.h>

#ifdef BENCHMARK
#	include <sys/timeb.h>
#endif

#define FCA_NAME	"ffss-client"
#define FCA_VERSION	"0.5.2"
#define FCA_AUTHOR	"bennyben"

#define FCA_MAX_CMD		10
#define FCA_VAR_MAX		64
#define FCA_MAX_POSS_VALUES	16

#ifndef _WIN32  /* not win */
#	define FCA_LAUNCH_SHELL_CMD     "$SHELL"
#	define FCA_PATH_SEPARATOR	"/"
#else		/* win :( */
#	error argggg...je sais pas trop comment lancer un shell sous win (command ou cmd sous 2000 ?)
#	define FCA_PATH_SEPARATOR	"\\"
#endif

	/* little problem with skyutils (old version?) */
#ifdef SU_strcasecmp
	/* SU_strcasecmp returns the inverse of strcasecmp */
#	define SU_strcasecmp !SU_strcasecmp
#else
#	define SU_strcasecmp strcasecmp
#endif

#include "common.h"
#include "skin.h"

    /* functions */
void FCA_init();
void FCA_process_args();
#ifdef CGI
void FCA_process_cgi_args();
#endif /* CGI */
void FCA_first_list();
void FCA_uninit();

int main(int argc, char ** argv);

    /*
     *		globals
     */
extern char FCA_pwd[FFSS_MAX_PATH_LENGTH];
extern char FCA_conSh[FFSS_MAX_PATH_LENGTH];	/* the connected share */
extern char FCA_conIP[FFSS_IP_FIELD_SIZE];	/* the connected server */
extern SU_PClientSocket FCA_shrSkt;
extern char FCA_tolist_dir[FFSS_MAX_PATH_LENGTH];
extern const FCA_Tcommand FCA_COMMANDS[];
extern const FCA_Tvar FCA_VARS[];
extern const char *FCA_VAR_VALUES[][FCA_MAX_POSS_VALUES];
extern char FCA_env[][FCA_VAR_MAX];
extern char FCA_home[FFSS_MAX_DOMAIN_LENGTH];
extern FCA_Pskin FCA_skin;
extern bool FCA_exiting;
#ifdef BENCHMARK
extern struct timeb FCA_starttime, FCA_stoptime;
#endif

#define FC_DBGMSG_GLOBAL 0x00010

#endif
