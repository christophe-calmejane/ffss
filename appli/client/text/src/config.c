/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	config file management
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

#include "config.h"
#include "client.h"
#include "args.h"
#include "display.h"
#include "command.h"

	/* GLOBALS */
bool FCA_reading_file;
char FCA_file_status[FCA_MAX_FILE_STATUS];

	/* the config file's locations */
char *FCA_CONFIG_FILES[]={
	"/usr/local/share/ffss/clientrc",	/* deprecated */
	"/usr/local/share/ffss/ffss-clientrc",	/* deprecated */
	"/usr/local/share/ffss/ffss-client.conf",/* deprecated */
	"/usr/local/share/ffss/Client.conf",	/* deprecated */

	"/usr/share/ffss/clientrc",		/* deprecated */
	"/usr/share/ffss/ffss-clientrc",	/* deprecated */
	"/usr/share/ffss/ffss-client.conf",	/* deprecated */
	"/usr/share/ffss/Client.conf",		/* deprecated */

	"/etc/ffss/clientrc",
	"/etc/ffss//ffss-clientrc",
	"/etc/ffss/ffss-client.conf",		/* deprecated */
	"/etc/ffss/Client.conf",		/* deprecated */

	"/usr/local/etc/ffss/clientrc",
	"/usr/local/etc/ffss/ffss-clientrc",
	"/usr/local/etc/ffss/ffss-client.conf",	/* deprecated */
	"/usr/local/etc/ffss/Client.conf",	/* deprecated */
#ifdef __unix__
	"$.ffss-client",
	"$.ffss-clientrc",			/* deprecated */
	"$.ffss-client.conf",			/* deprecated */
#endif
	"ffss-clientrc",
	"clientrc",
	"ffss-client.conf",			/* deprecated */
	"Client.conf",				/* deprecated */
	NULL
};

void FCA_read_file(FILE *fp, char *filename);


void FCA_read_cfg()
{
	FILE *fp=NULL;
	char *filename=NULL;
	char tmp[FCA_MAX_HOME];
	int ic=0;
	char *home;

#ifdef __unix__
	home=getenv("HOME");
#endif
	while(FCA_CONFIG_FILES[ic]) {
		while(!fp && FCA_CONFIG_FILES[ic]) {
			filename=FCA_CONFIG_FILES[ic];
#ifdef __unix__
				/* '$' before the name=
					$HOME/ */
			if(*filename=='$') {
				snprintf(tmp,FCA_MAX_HOME, "%s/%s", home, FCA_CONFIG_FILES[ic]+1);
				filename=tmp;
			}
#endif
			SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) trying file: '%s'", filename);
			fp=fopen(filename, "rt");
			ic++;
		}
		if(fp) {
			SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) using file: '%s'", filename);
			FCA_read_file(fp,filename);
			fclose(fp);
			fp=NULL;
		}
	}

	fp=fopen(FCA_args.cfg_file, "rt");
	filename=FCA_args.cfg_file;
	if(fp) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) using file: '%s'", filename);
		FCA_read_file(fp,filename);
		fclose(fp);
	}

}

void FCA_read_file(FILE *fp, char *filename)
{
	int ic=1;

		/* let's read */
	FCA_reading_file=true;
	FCA_command=malloc(FCA_CMD_MAX*sizeof(char));
	while( SU_ReadLine(fp, FCA_command, FCA_CMD_MAX) ) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) read: '%s'", FCA_command);
		snprintf(FCA_file_status,FCA_MAX_FILE_STATUS,"file %s, line %d", filename, ic++);
		FCA_interpret_cmd();
		if(FCA_command)
			free(FCA_command);
		FCA_command=malloc(FCA_CMD_MAX*sizeof(char));
	}
	if(FCA_command)
			free(FCA_command);
	FCA_command=NULL;
	FCA_reading_file=false;
}
