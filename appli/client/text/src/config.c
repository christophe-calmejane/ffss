/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	config file management
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
	"/usr/local/share/ffss/clientrc",
	"/usr/local/share/ffss/Client.conf",
	
	"/usr/share/ffss/clientrc",
	"/usr/share/ffss/Client.conf",
	
	"/etc/ffss/clientrc",
	"/etc/ffss/Client.conf",
	
	"/usr/local/etc/ffss/clientrc",
	"/usr/local/etc/ffss/Client.conf",
#ifdef __unix__	
	"$.ffss-clientrc",
	"$.ffss-client.conf",
	"$.ffss-client",
#endif	
	"ffss-clientrc",
	"ffss-client.conf",
	"clientrc",
	"Client.conf",
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
			FFSS_PrintDebug(6, "(client) trying file: '%s'\n", filename);
			fp=fopen(filename, "rt");
			ic++;
		}
		if(fp) {
			FFSS_PrintDebug(3, "(client) using file: '%s'\n", filename);
			FCA_read_file(fp,filename);
			fclose(fp);
			fp=NULL;
		}
	}
	
	fp=fopen(FCA_args.cfg_file, "rt");
	filename=FCA_args.cfg_file;
	if(fp) {
		FFSS_PrintDebug(3, "(client) using file: '%s'\n", filename);
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
		FFSS_PrintDebug(5, "(client) readen: '%s'\n", FCA_command);
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
