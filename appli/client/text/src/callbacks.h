/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	callbacks from the library
 */

#ifndef _FCA_CALLBACKS_H_
#define _FCA_CALLBACKS_H_

#include <ffss.h>

    /* UDP */
void FCA_OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
void FCA_OnBeginTCPThread(SU_PClientSocket Server);
void FCA_OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList);
void FCA_OnEndServerListingAnswer(void);
void FCA_OnDomainListingAnswer(const char **Domains,int NbDomains);
void FCA_OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares);
void FCA_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,int NbAnswers);
void FCA_OnMasterError(int Code,const char Descr[]);
    /* TCP */
bool FCA_OnError(SU_PClientSocket Server,int Code,const char Descr[],FFSS_LongField Value);
bool FCA_OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries);
void FCA_OnEndTCPThread(SU_PClientSocket Server);
void FCA_OnIdleTimeout(SU_PClientSocket Client);
void FCA_OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download);
void FCA_OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
void FCA_OnTransfertSuccess(FFSS_PTransfer FT,bool Download);
FFSS_PTransfer FCA_OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag);
FFSS_PTransfer FCA_OnData(SU_PClientSocket Server,FFSS_Field XFerTag);
    /* fatal error */
void FCA_OnFatalError(void);
void FCA_OnUDPError(int ErrNum);

#endif
