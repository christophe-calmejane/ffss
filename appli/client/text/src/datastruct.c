/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	structures management
 */

#include "command.h"
#include "display.h"
#include "datastruct.h"
#include "client.h"

	/* GLOBALS */
SU_PList FCA_Servers;   /* pointer to the structure */

SU_PList FCA_list;       /* the other structure, listing of current dir */
char FCA_listed_dir[FFSS_MAX_FILEPATH_LENGTH];

#ifdef ALIASES
SU_PList FCA_aliases;	/* command aliases */
#endif

void FCA_free_Domains()
{
	/* free all domains, but don't free the domain list... */
    SU_PList Pdom;
    FCA_PServs Pser;
    SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free-ing all domains");

    Pdom=FCA_Servers;
    while(Pdom!=NULL) {
	Pser=(FCA_PServs)SU_GetElementHead(Pdom);
	FCA_free_servs(Pser->servers);
	    /* data are freed, but this pointeur must be initialised */
	Pser->servers=NULL;
	Pdom=Pdom->Next;
    }
    SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free all domains OK");
}

void FCA_free_Domain_list()
{
	/* call FCA_free_Domains before !!!! */
    if(FCA_Servers==NULL)
	return;
    SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free-ing domain list");
    SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free(%s)", ((FCA_PServs)FCA_Servers->Data)->Domain );
    free(FCA_Servers->Data);
    FCA_Servers=SU_DelElementHead(FCA_Servers);
    FCA_free_Domain_list();
}

void FCA_free_Domain(const char *Domain)
{
    SU_PList Ptr;
    FCA_PServs servs=NULL;
    bool found=false;

    Ptr=FCA_Servers;

     SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free-ing domain: %s", Domain);

    while(Ptr!=NULL && !found) {
	servs=(FCA_PServs)SU_GetElementHead(Ptr);
	found=!SU_strcasecmp(servs->Domain, Domain);
	Ptr=Ptr->Next;
    }
    if(found)
	FCA_free_servs(servs->servers);
     SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free domain OK");
}

void FCA_free_servs(SU_PList Servers)
{
	/* free recursively servers in a domain */
    FM_PHost servs;

    if(Servers==NULL)
	return;
    FCA_free_servs(Servers->Next);
     SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free-ing all servers");
    servs=(FM_PHost)SU_GetElementHead(Servers);
     SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free(%s)", servs->Name);
    free(servs->Name);
    free(servs->OS);
    free(servs->Comment);
    free(servs->IP);
    free(servs);
    free(Servers);
}

SU_PList FCA_get_Domain(const char *Domain)
{
		/* look for a domain... */
	SU_PList PServs;
	FCA_PServs serv=NULL;
	bool found=false;

	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) looking for domain: %s", Domain);

		/* never listed domains */
	if(!FCA_everListDom) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) never listed domains");
		FCA_everListDom=true;
		FCA_quiet=true;
		FCA_list_domains();
		FCA_quiet=false;
	}
	PServs=FCA_Servers;
		/* no domain */
	if(PServs==NULL)
		return NULL;

	serv=(FCA_PServs)SU_GetElementHead(PServs);
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) >%s<->%s<", serv->Domain, Domain);
	found=!SU_strcasecmp(serv->Domain, Domain);
	while(PServs->Next!=NULL && !found) {
		PServs=PServs->Next;
		serv=(FCA_PServs)SU_GetElementHead(PServs);
		found=!SU_strcasecmp(serv->Domain, Domain);
	}

	if(!found) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) domain not found");
		return NULL;
	}
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) domain found");
	return PServs;
}

SU_PList FCA_get_server(SU_PList Domain, const char *machine)
{
		/* search a server in a domain */
	SU_PList Pserv, Pbest=NULL;
	FM_PHost host;

	if(Domain==NULL)
		return NULL;
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) looking for server in the domain: %s", machine);
		/* if this domain has never been listed */
	if(! ((FCA_PServs)(Domain->Data))->everlisted ) {
		if( SU_strcasecmp( ((FCA_PServs)(Domain->Data))->Domain , "None") ) {
			SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) this domain has never been listed");
			FCA_quiet=true;
			((FCA_PServs)(Domain->Data))->everlisted=true;
			FCA_list_servs(((FCA_PServs)(Domain->Data))->Domain);
			FCA_quiet=false;
		} else
			SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) 'None' domain has never been listed...");
	}
	Pserv=Domain;
	Pserv=( (FCA_PServs)(Pserv->Data) )->servers;
	while(Pserv!=NULL) {
	    	host=(FM_PHost)(Pserv->Data);
		if( ((!SU_strcasecmp(host->IP, machine)) || (!SU_strcasecmp(host->Name, machine)))
		  && (!Pbest || host->State!=FFSS_STATE_OFF) )
			Pbest=Pserv;
		Pserv=Pserv->Next;
	}
	if(!Pbest) {
		 SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) server not found");
		return NULL;
	}
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) server found");
	return Pbest;
}

FM_PHost FCA_get_host(const char *domain, const char* machine)
{
		/*  get_server: from pointer to domain
		    get_host:   from domain name

		search machine in the structure, and return his IP if found, NULL else
		 */
	SU_PList Pservs=NULL;
	FM_PHost serv=NULL;
	bool found=false;

	Pservs=FCA_get_Domain(domain);
	if(Pservs==NULL)
		return NULL;
	Pservs=( (FCA_PServs)(Pservs->Data) )->servers;

		/* find the server in the list */
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) searching machine: %s", machine);
	while(!found && Pservs!=NULL) {
		serv=(FM_PHost)(Pservs->Data);
		if( !SU_strcasecmp(machine, serv->Name) || !SU_strcasecmp(machine, serv->IP) )
			found=true;
		Pservs=Pservs->Next;
	}
	if(found) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) found IP: %s", serv->IP);
		return serv;
	}
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) not found");
	return NULL;
}

SU_PList FCA_add_Domain(const char *Domain)
{
	/* insert a new moo-domain */
	FCA_PServs dom;

	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) adding domain: %s", Domain);
	dom=malloc(sizeof(FCA_TServs));
	if(dom==NULL)
		FCA_crash("out of memory");

	dom->Domain=strdup(Domain);
	dom->servers=NULL;
	dom->everlisted=false;
	FCA_Servers=SU_AddElementHead(FCA_Servers, dom);
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) domain added OK");
	return FCA_Servers;
}

char *FCA_get_a_domain()
{
	static SU_PList p=NULL;
	static bool isStarted=false;
	char *r;

		/* returns a domain each time it is called
		 at the end of the domain list, returns NULL
		*/
	if(!p) {
		p=FCA_Servers;
		if(!p)
			return NULL;
		if(!isStarted)
			isStarted=true;
		else
			return NULL;
	}
	r=((FCA_PServs)(p->Data))->Domain;
	p=p->Next;
	return r;
}


void FCA_free_list()
{
	SU_PList P=FCA_list;

	if(FCA_list) {
		FCA_list=P->Next;
		FCA_free_list();
		free( ( (FC_PEntry)(P->Data) )->Name );
		free(P->Data);
		free(P);
	}
}

SU_PList FCA_find_el(char *name)
{
	SU_PList P=FCA_list;

	while(P && strcmp( ((FC_PEntry)(P->Data))->Name, name))
		P=P->Next;
	return P;
}

#ifdef ALIASES
	/* just add an alias (alias cmd=cmd2) */
void FCA_add_alias(char *cmd, char *val)
{
	FCA_Palias a;
	SU_PList l;

		/* first, we must verify if there's another existing alias  */
	l=FCA_aliases;
	while( l && strcmp(( (FCA_Palias)(l->Data) )->cmd,cmd) )
		l=l->Next;
	if(l) {		/* another alias, replace it */
		free(( (FCA_Palias)(l->Data) )->val);
		( (FCA_Palias)(l->Data) )->val=strdup(val);
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) alias replaced");
	} else {	/* new alias */
		a=malloc(sizeof(FCA_Talias));

		a->cmd=strdup(cmd);
		a->val=strdup(val);
		FCA_aliases=SU_AddElementHead(FCA_aliases,(void*)a);
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) alias added");
	}
}

	/* del an alias (unalias cmd) */
bool FCA_del_alias(char *cmd)
{
	SU_PList p;

	p=FCA_aliases;
	while( p && strcmp(( (FCA_Palias)(p->Data) )->cmd,cmd) )
		p=p->Next;
	if(!p)	/* nothing to delete */
		return false;
	free(((FCA_Palias)(p->Data))->cmd);
	free(((FCA_Palias)(p->Data))->val);
	FCA_aliases=SU_DelElementElem(FCA_aliases,p->Data);
	free(p->Data);
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) alias removed");
	return true;
}

	/* get the susbtitute to the command if it's an alias, for ex:
		from 'll', we get 'ls -l' */
char *FCA_get_alias(char *cmd)
{
	SU_PList p;

	p=FCA_aliases;
	while( p && strcmp(( (FCA_Palias)(p->Data) )->cmd,cmd) )
		p=p->Next;
	if(!p)
		return NULL;
	return ( (FCA_Palias)(p->Data) )->val;
}

void FCA_free_aliases()
{
	SU_PList p=FCA_aliases;

	if(!p)	return;
	FCA_aliases=p->Next;
	FCA_free_aliases();
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) free(alias '%s')", ( (FCA_Palias)(p->Data) )->cmd);
	free(( (FCA_Palias)(p->Data) )->cmd);
	free(( (FCA_Palias)(p->Data) )->val);
	free(p->Data);
	free(p);
}
#endif	/* aliases */
