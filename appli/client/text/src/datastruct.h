/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	structures management
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

#ifndef _FCA_DATASTRUCT_H_
#define _FCA_DATASTRUCT_H_

#include <ffss.h>

	/* servers and domains structure */
typedef struct {
    char *Domain;
    bool everlisted;
    SU_PList servers;
} FCA_TServs, *FCA_PServs;

#ifdef ALIASES
typedef struct {
	char *cmd;
	char *val;
} FCA_Talias, *FCA_Palias;
#endif

extern SU_PList FCA_Servers;	/* pointer to the structure */


void FCA_free_Domains();
void FCA_free_Domain_list();
void FCA_free_Domain(const char *Domain);
void FCA_free_servs(SU_PList Servers);
SU_PList FCA_get_Domain(const char *Domain);
SU_PList FCA_get_server(SU_PList Domain, const char *machine);
FM_PHost FCA_get_host(const char *domain, const char *machine);
SU_PList FCA_add_Domain(const char *Domain);
char *FCA_get_a_domain();

		/* listing structure */
extern SU_PList FCA_list;
extern char FCA_listed_dir[FFSS_MAX_FILEPATH_LENGTH];

void FCA_free_list();
SU_PList FCA_find_el(char *name);

#ifdef ALIASES
	/* aliases */
extern SU_PList FCA_aliases;

void FCA_add_alias(char *cmd, char *val);
bool FCA_del_alias(char *cmd);
char *FCA_get_alias(char *cmd);
void FCA_free_aliases();
#endif	/* aliases */

#endif	/* _FCA_DATASTRUCT_H_ */
