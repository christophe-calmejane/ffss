/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	script skin
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

void FCA_scr_size(unsigned int n, const char text[]);

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

void FCA_scr_size(unsigned int n, const char text[])
{
	printf("%d", n);
}
