/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	config file management
 */

#ifndef _FCA_CONFIG_H_
#define _FCA_CONFIG_H_

    /* it's ok, there's all in this :) */
#include <ffss.h>

#define FCA_CFG_MASTER		"master"
#define FCA_CFG_CAN_ANSI	"ansi"
#define FCA_CFG_DEBUG		"debug"

#define FCA_MAX_FILE_STATUS	1024
#define FCA_MAX_HOME		512

    /* globals */
extern char *FCA_CONFIG_FILES[];

extern bool FCA_reading_file;
extern char FCA_file_status[FCA_MAX_FILE_STATUS];

void FCA_read_cfg();

#endif /* _FCA_CONFIG_H_ */
