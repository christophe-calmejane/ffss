/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	script skin
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

#include <stdio.h>
#include "skin.h"
#include "client.h"

void FCA_scr_tab_stitle(const char name[], const unsigned int size);
void FCA_scr_tab_int_stitle();
void FCA_scr_tab_post_stitle();

void FCA_scr_tab_item(const char name[], const unsigned int size);
void FCA_scr_tab_int_item();
void FCA_scr_tab_post_item();

void FCA_scr_size(FFSS_LongField n, const char text[]);

FCA_Tps FCA_scr_ps;


void FCA_script_init()
{
	FCA_scr_ps.tab_stitle=FCA_scr_tab_stitle;
	FCA_scr_ps.tab_int_stitle=FCA_scr_tab_int_stitle;
	FCA_scr_ps.tab_post_stitle=FCA_scr_tab_post_stitle;

	FCA_scr_ps.tab_item=FCA_scr_tab_item;
	FCA_scr_ps.tab_int_item=FCA_scr_tab_int_item;
	FCA_scr_ps.tab_post_item=FCA_scr_tab_post_item;

	FCA_scr_ps.size=FCA_scr_size;
}


void FCA_scr_tab_stitle(const char name[], const unsigned int size)
{
	printf("%s", name);
}

void FCA_scr_tab_int_stitle()
{
	printf("%c", FCA_script_delim);
}

void FCA_scr_tab_post_stitle()
{
	printf("\n");
}
void FCA_scr_tab_item(const char name[], const unsigned int size)
{
	printf("%s", name);
}

void FCA_scr_tab_int_item()
{
	printf("%c", FCA_script_delim);
}

void FCA_scr_tab_post_item()
{
	printf("\n");
}

void FCA_scr_size(FFSS_LongField n, const char text[])
{
	printf("%lld", n);
}
