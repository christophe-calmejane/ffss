/*
 *	FFSS client
 * 
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	callbacks from the library
 */

#include "callbacks.h"
#include "display.h"
#include "client.h"
#include "common.h"
#include "command.h"

void FCA_OnBeginTCPThread(SU_PClientSocket Server)
{
	FFSS_PrintDebug(1, "(client) Thread is started\n");
}

    /* new machine found */
void FCA_OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
	if(FCA_inDispServs) {
		FFSS_PrintDebug(1, "(client) received a new state from %s\n", IP);
		FCA_print_state(State, IP, Domain, Name, OS, Comment, MasterIP);
	}
}

void FCA_OnDomainListingAnswer(const char **Domains,int NbDomains)
{
	FFSS_PrintDebug(1, "(client) received a domain listing\n");
	FCA_print_domains(Domains, NbDomains);
	    	/* end of domains, we stop the domain listing */
	FCA_sem_post();
}

/* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped !!! */
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void FCA_OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList)
{
	FFSS_PrintDebug(1, "(client) received a server listing for domain %s\n", Domain);
	FCA_print_servers(Domain, NbHost, HostList);

	/* no sem_post, this callback is called for each domain */
}

void FCA_OnEndServerListingAnswer(void)
{
	FFSS_PrintDebug(1, "(client) End of server listing\n");
	FCA_inDispServs=false;
		/* end of domains, we stop the server listing */
	FCA_sem_post();
}

    /* new ls */
bool FCA_OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries)
{
	FFSS_PrintDebug(3, "(client) received a dir listing for path %s\n", Path);
	FCA_print_ls(Path, NbEntries,Entries);

	FCA_sem_post();
	return true;
}
    /* listing shares */
void FCA_OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares)
{
	FFSS_PrintDebug(3, "(client) received a share listing from %s\n", IP);
	FCA_print_shares(IP,Names,Comments,NbShares);

/* only for debug 
sleep(5);
printf("POUF\n");
*/
	FCA_sem_post();
}

void FCA_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,int NbAnswers)
{
	FFSS_PrintDebug(3, "(client) received search answer for domain %s, query=%s\n", Domain, Query);
	
	if(FCA_inDispFind || !FCA_multiFind)
		FCA_print_search(Query,Domain,Answers,NbAnswers);
	else
		FFSS_PrintDebug(3, "(client) sorry, too late to give your answer\n");
    
    	if(!FCA_multiFind)
		FCA_sem_post();
}

void FCA_OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download)
{
	FCA_dw_amount+=Amount;
	if(!FCA_quiet)
		FCA_progr_bar();
}

void FCA_OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
	if(!FCA_quiet)
		printf("\n");
	FCA_quiet=false;
	FCA_print_cmd_err("download failed...");
	FCA_print_cmd_err("(error code %d, %s)\n", (int)ErrorCode, Error);
	FCA_Ptrans=NULL;
    
	FCA_sem_post();
}

void FCA_OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
	FFSS_PrintDebug(3, "(client) download ended without any error\n");
	FCA_print_dw_ok(FCA_dw_file, 12.2);
	FCA_Ptrans=NULL;

/*FFSS_PrintDebug(3, "(client) sleeeeeeeep\n");
	usleep(2000000);
FFSS_PrintDebug(3, "(client) wake up baby\n");
*/
	FCA_sem_post();
}

void FCA_OnMasterError(int Code,const char Descr[])
{
	FCA_print_cmd_err("error from master (code %d, %s)\n", Code, Descr);

	FCA_sem_post();
}

    /* error */
bool FCA_OnError(SU_PClientSocket Server,int Code,const char Descr[])
{
	FFSS_PrintDebug(1, "(client) error message recieved: Code: %d (%s)\n", Code, Descr);
	if(Code==FFSS_ERROR_XFER_MODE_NOT_SUPPORTED) {
		FCA_print_warning("This server supports only useconnsock mode, switching to this one");
		sprintf(FCA_useConnSock, "on");
		FCA_err_errno=Code;
	} else {
		if(! FCA_quiet)
			FCA_print_conn_err(Server, Code, Descr);
		if(Code!=FFSS_ERROR_NO_ERROR) {	/* if error during share connection */
			/* connection altered */
			if( Code!=FFSS_ERROR_FILE_NOT_FOUND && Code!=FFSS_ERROR_ACCESS_DENIED )
				FCA_close_connection();
			FCA_err_errno=Code;
		}
	}
	FCA_sem_post();
	return true;
}
    /* connection lost */
void FCA_OnEndTCPThread(SU_PClientSocket Server)
{
	char *domain, *machine, *share, *dir;

	FFSS_PrintDebug(1, "(client) connection closed to this share, thread stopped\n");
	FCA_close_connection();
	if(FCA_canChange_pwd) {
			/* update pwd, and go on /$/domain/machine */
		FCA_explode_path(FCA_pwd, &domain, &machine, &share, &dir);
		if(domain!=NULL && machine!=NULL)
			*(machine-1)='/';
	}
	
	FCA_sem_post();
}
    /* too many UDP errors: fatal error, exits */
void FCA_OnFatalError(void)
{
	FFSS_PrintDebug(1, "(client) too many UDP errors\n");
	if(!FCA_exiting)
		FCA_crash("too many UDP errors, exiting");
}
    
void FCA_OnUDPError(int ErrNum)
{
	FFSS_PrintDebug(1, "(client) UDP error code %d\n", ErrNum);
	FCA_UDP_errno=ErrNum;
    
	FCA_sem_post();
}

void FCA_OnIdleTimeout(SU_PClientSocket Client)
{
	FCA_print_cmd_err("IDLE timeout, disconnected");

	FCA_sem_post();
}

FFSS_PTransfer FCA_OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag)
{
	return FCA_Ptrans;
}

FFSS_PTransfer FCA_OnData(SU_PClientSocket Server,FFSS_Field XFerTag)
{
	return FCA_Ptrans;
}
