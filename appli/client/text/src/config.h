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
