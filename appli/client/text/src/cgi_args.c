/* 
 *	FFSS client
 *		
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	CGI functions
 */

#include <stdlib.h>
/* libCGI
#include <cgi.h>
*/
#include <ffss.h>
#include <skyutils.h>

#include "cgi_args.h"
#include "common.h"
#include "client.h"

#ifdef CGI
	/* to use CGI mode, we MUST have installed the
		html skin
	*/
#include "skin.h"
#ifndef FCA_html_prefix
#	error "to use CGI, you must have the HTML skin. FCA_html_prefix is not defined in skin.h"
#endif /* FCA_html_prefix */
#ifndef FCA_html_img_prefix
#	error "to use CGI, you must have the HTML skin. FCA_html_img_prefix is not defined in skin.h"
#endif /* FCA_html_img_prefix */
#ifndef FCA_html_firstarg
#	error "to use CGI, you must have the HTML skin. FCA_html_firstarg is not defined in skin.h"
#endif /* FCA_html_firstarg */
#ifndef FCA_html_dw_prefix
#	error "to use CGI, you must have the HTML skin. FCA_html_dw_prefix is not defined in skin.h"
#endif /* FCA_html_dw_prefix */
#ifndef FCA_me
#	error "to use CGI, you must have the HTML skin. FCA_me is not defined in skin.h"
#endif /* FCA_html_dw_prefix */
#endif /* CGI */


bool FCA_CGI_mode;

#ifdef CGI
	/* CGI-specific variables */
#ifdef CGI_DOWNLOADS
bool FCA_must_download;
#endif
bool FCA_can_header;
char FCA_dir_to_list[FFSS_MAX_PATH_LENGTH];
char FCA_search[FFSS_MAX_KEYWORDS_LENGTH];
char FCA_search_dom[FFSS_MAX_DOMAIN_LENGTH];

	/* if you add a cgi arg that is in the client environment,
		update the FCA_read_cgi_args() too
	   if you want to rename an argument, don't forget to
		update the skin_html.c, FCA_form_hidden_args()
		and FCA_post_link(bool firstArg)
	*/
const FCA_TCGI_arg FCA_CGI_ARGS[]={
/*
	name		variable		max of the	index in the FCA_VARS table
			where it's stored	 variable	 (only if the value is restricted)
*/
	{"prefix",	FCA_html_prefix,	FCA_VAR_MAX,		-1	},
	{"img_prefix",	FCA_html_img_prefix,	FCA_VAR_MAX,		-1	},
	{"firstarg",	FCA_html_firstarg,	FCA_VAR_MAX,		FCA_skin_env_index+2},
	{"skin",	FCA_skin_name,		FCA_VAR_MAX,		-1	},
	{"dir",		FCA_dir_to_list,	FFSS_MAX_PATH_LENGTH,	-1	},
	{"s",		FCA_search,		FFSS_MAX_KEYWORDS_LENGTH,-1	},
	{"sdom",	FCA_search_dom,		FFSS_MAX_DOMAIN_LENGTH,	-1	},
	{"master",	FCA_master,		FCA_VAR_MAX,		-1	},
	{"debug",	FCA_debuglevel,		FCA_VAR_MAX,		1	},
#ifdef CGI_DOWNLOADS
	{"dw_prefix",	FCA_html_dw_prefix,	FCA_VAR_MAX,		-1	},
#endif
	{"me",		FCA_me,			FCA_VAR_MAX,		-1	},
	{NULL,		NULL,			FCA_VAR_MAX,		-1	}
};

const FCA_TCGI_bool_arg FCA_CGI_BOOL_ARGS[]={
/*
	name		variable
*/
#ifdef CGI_DOWNLOADS
	{"download",	&FCA_must_download	},
#endif
	{"can_header",	&FCA_can_header,	},
	{NULL,		NULL			}
};


	/* alternatives to libcgi bugged functions */
SU_PList FCA_cgi_vars;
int FCA_hextable[256];
typedef struct
{
	char *var;
	char *val;
} FCA_Tcgi_var, *FCA_Pcgi_var;

void FCA_process_form();
char *FCA_cgi_param(const char *var);
void FCA_process_form_data(char *query);
void FCA_free_cgi_params();
char *FCA_cgi_unescape_special_chars(char *str);
void FCA_init_hextable();


	/* CGI-specific functions */
void FCA_read_cgi_args()
{
	char *p;
	const FCA_TCGI_arg *pa;
	const FCA_TCGI_bool_arg *pba;
	
	if(!FCA_CGI_mode)
		return;
		/* init all variables */
#ifdef CGI_DOWNLOADS
	FCA_must_download=false;
#endif
	FCA_can_header=true;
	FCA_dir_to_list[0]='\0';
	FCA_search[0]='\0';
	FCA_search_dom[0]='\0';
	
		/* I know it isn't good to put init_hextable here... */
	FCA_init_hextable();
		/* read args */
	FCA_process_form();
	pa=FCA_CGI_ARGS;
	while(pa->name) {
		p=FCA_cgi_param(pa->name);
		if(p) {
			if(pa->env_index!=-1) {
				if(FCA_if_val_ok(pa->env_index, p)) {

					SU_strcpy(pa->var, p, pa->max);
					if(FCA_VARS[pa->env_index].onChange)
						FCA_VARS[pa->env_index].onChange();
				} else
					FCA_print_err("%s n'est pas une valeur acceptee pour la variable %s", p, pa->name);
			} else
				SU_strcpy(pa->var, p, pa->max);
		}
		pa++;
	}
		/* read bool args */
	pba=FCA_CGI_BOOL_ARGS;
	while(pba->name) {
		p=FCA_cgi_param(pba->name);
		if(p) {
			if( !strcmp(p, "0") )
				*(pba->var)=false;
			else if( !strcmp(p, "1") )
				*(pba->var)=true;
			else
				FCA_print_err("%s n'est pas une valeur acceptee pour la variable %s", p, pba->name);
		}
		pba++;
	}
	FCA_free_cgi_params();
}

	/* just this, because libcgi can be VERY unstable
		these functions will be deprecated when
		libcgi will be stable
		
		functions adapted from libcgi
	*/

	/* copied/rewritten, as you want */
void FCA_init_headers()
{
	if(FCA_can_header)
		printf("Content-type: text/html\n\n");
}

	/* copied from scratch */
void FCA_init_download_headers()
{
		/* it's an unknown MIME type,
			temporary because I don't know
			now how to get the MIME type
			from a file name
		 */
	printf("Content-type: ffss/download\n\n");
}

	/* copied, and rewritten */
void FCA_process_form()
{
	char *meth, *q, *clen;
	int len;
	
	meth=getenv("REQUEST_METHOD");
	if(!meth)
		return;
	if (!strcasecmp("POST", meth)) {
			/* POST = read all the document */
		clen = getenv("CONTENT_LENGTH");
		if(!clen)
			return;
		len = atoi(clen);
		if(len<1)
			return;
		q = malloc(len * sizeof(char));
		if (!q)
			FCA_crash("not enough memory");
		fread(q, len, 1, stdin);
		FCA_process_form_data(q);
	} else if (!strcasecmp("GET", meth))  {
			/* GET = all in QUERY_STRING */
		q=getenv("QUERY_STRING");
		if (!q || q[0]=='\0')
			return;
		FCA_process_form_data(q);
	}
}

	/* rewritten from scratch */
void FCA_process_form_data(char *query)
{
	char *eq, *pq=query, *l;
	FCA_Pcgi_var v;
	
	FCA_cgi_vars=NULL;
	
	while( pq && (eq=strchr(pq, '=')) ) {
			/* non-empty variable and values */
		l=strchr(pq, '&');
		if( pq!=eq && *(eq+1)!='\0' && pq!=l && l!=eq+1) {
			*eq='\0';
			if(l)
				*l='\0';
				/* var=pq
				   val=eq+1
				 */
			v=malloc(sizeof(FCA_Tcgi_var));
			v->var=strdup(pq);
				/* it's like strdup */
			v->val=FCA_cgi_unescape_special_chars(eq+1);
			/* debug 
			printf("var='%s' val='%s'<br>\n", v->var, v->val);
			*/
			FCA_cgi_vars=SU_AddElementHead(FCA_cgi_vars, (void*)v);
				/* variable added , restore the QUERY_STRING */
			*eq='=';
			if(l)
				*l='&';
		}
		pq=l;
		if(pq)	pq++;
	}
}

	/* rewritten from scratch */
char *FCA_cgi_param(const char *var)
{
	SU_PList p;
	
		/* look for the variable in the variable list
			and returns its value
		*/
	if(!FCA_cgi_vars)
		return NULL;
	p=FCA_cgi_vars;
	while( p && strcmp( ((FCA_Pcgi_var)(p->Data))->var,var) )
		p=p->Next;
	if(!p)
		return NULL;
	return ((FCA_Pcgi_var)(p->Data))->val;
}

	/* rewritten from scratch */
void FCA_free_cgi_params()
{
	SU_PList p=FCA_cgi_vars;
	
	if(!p)	return;
	FCA_cgi_vars=p->Next;
	FCA_free_cgi_params();
	free( ((FCA_Pcgi_var)(p->Data))->var );
	free( ((FCA_Pcgi_var)(p->Data))->val );
	free(p->Data);
	free(p);
}

	/* copied from libcgi, added some code */
char *FCA_cgi_unescape_special_chars(char *str)
{
	char *tmp;
	register int i, len, pos = 0;
	
	len = strlen(str);
		/* len -> len+1 (forgotten the '\0') */
	tmp = (char *)malloc((len+1) * sizeof(char));
	if (!tmp)
		return NULL;

	for (i = 0; i < len; i++) {
		if (str[i] == '%') 
			tmp[pos++] = (FCA_hextable[(int)str[++i]] << 4) + FCA_hextable[(int)str[++i]];
		else if (str[i] == '+') 
			tmp[pos++] = ' ';
		else 
			tmp[pos++] = str[i];
	}
		/* added */
	tmp[pos]='\0';

	return tmp;
}

	/* copied from libcgi, just to have an hextable */
void FCA_init_hextable()
{
	register int i;
	for (i = 0; i < 256; i++)
		FCA_hextable[i] = 0;

	FCA_hextable['1'] = 1;
	FCA_hextable['2'] = 2;
	FCA_hextable['3'] = 3;
	FCA_hextable['4'] = 4;
	FCA_hextable['5'] = 5;
	FCA_hextable['6'] = 6;
	FCA_hextable['7'] = 7;
	FCA_hextable['8'] = 8;
	FCA_hextable['9'] = 9;
	FCA_hextable['a'] = 10;
	FCA_hextable['b'] = 11;
	FCA_hextable['c'] = 12;
	FCA_hextable['d'] = 13;
	FCA_hextable['e'] = 13;
	FCA_hextable['f'] = 15;
	FCA_hextable['A'] = 10;
	FCA_hextable['B'] = 11;
	FCA_hextable['C'] = 12;
	FCA_hextable['D'] = 13;
	FCA_hextable['E'] = 14;
	FCA_hextable['F'] = 15;
}

#endif	/* CGI */

/*
	copied from libcgi,
	Original code from PHP source (http://www.php.net)
*/
char *FCA_cgi_escape_special_chars(const char *str)
{
        unsigned char hex[] = "0123456789ABCDEF";
        register int i, j, len, tmp_len;
        unsigned char *tmp;

        len = strlen(str);
        tmp_len = len;
        tmp = (unsigned char*)malloc((len+1) * sizeof(char));
        if (!tmp)
                return NULL;

        for (i = 0, j = 0; i < len; i++, j++) {
                tmp[j] = (unsigned char)str[i];
                if (tmp[j] == ' ')
                        tmp[j] = '+';
                else if (!isalnum(tmp[j]) && strchr("_-.", tmp[j]) == NULL) {
                        tmp_len += 3;
                        tmp = realloc(tmp, tmp_len * sizeof(char));
                        if (!tmp)
                                return NULL;
                        tmp[j++] = '%';
                        tmp[j++] = hex[(unsigned char)str[i] >> 4];
                        tmp[j] = hex[(unsigned char)str[i] & 0x0F];
                }
        }
        tmp[j] = '\0';

	return tmp;
}

bool FCA_isInCGImode()
{
#ifdef CGI
	if(getenv("REQUEST_METHOD"))
		return true;
#endif
	return false;
}
