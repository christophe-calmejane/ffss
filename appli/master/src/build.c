#include "master.h"

/* Returns a buffer to be sent then freed, or NULL if queue is empty */
char *FM_BuildStatesBuffer(SU_PList Queue,long int *size_out)
{
  char *buf;
  SU_PList Ptr;
  long int len,nb,pos;

  nb = SU_ListCount(Queue);
  if(nb == 0)
    return NULL;
  len = sizeof(FFSS_Field) + nb * (sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES_2 + FFSS_IP_FIELD_SIZE*2 + FFSS_MAX_DOMAIN_LENGTH+1 + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1);
  buf = (char *) malloc(len);
  pos = 0;

  /* Flush number of entries */
  *(FFSS_Field *)(buf+pos) = nb;
  pos += sizeof(FFSS_Field);

  /* Flush State Queue */
  Ptr = Queue;
  while(Ptr != NULL)
  {
    /* Flush State */
    *(FFSS_Field *)(buf+pos) = ((FM_PQueue)Ptr->Data)->Host->State;
    pos += sizeof(FFSS_Field);
    /* Flush Master IP */
    *(FFSS_Field *)(buf+pos) = FFSS_IP_TYPE;
    pos += sizeof(FFSS_Field);
    FFSS_PackIP(buf+pos,((FM_PQueue)Ptr->Data)->Domain->Master,FFSS_IP_TYPE);
    pos += FFSS_IP_FIELD_SIZE;
    /* Flush Domain */
    len = strlen(((FM_PQueue)Ptr->Data)->Domain->Name)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    SU_strcpy(buf+pos,((FM_PQueue)Ptr->Data)->Domain->Name,len);
    pos += len;
    /* Flush Name */
    len = strlen(((FM_PQueue)Ptr->Data)->Host->Name)+1;
    if(len > FFSS_MAX_SERVERNAME_LENGTH)
      len = FFSS_MAX_SERVERNAME_LENGTH;
    SU_strcpy(buf+pos,((FM_PQueue)Ptr->Data)->Host->Name,len);
    pos += len;
    /* Flush OS */
    len = strlen(((FM_PQueue)Ptr->Data)->Host->OS)+1;
    if(len > FFSS_MAX_SERVEROS_LENGTH)
      len = FFSS_MAX_SERVEROS_LENGTH;
    SU_strcpy(buf+pos,((FM_PQueue)Ptr->Data)->Host->OS,len);
    pos += len;
    /* Flush Comment */
    len = strlen(((FM_PQueue)Ptr->Data)->Host->Comment)+1;
    if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
      len = FFSS_MAX_SERVERCOMMENT_LENGTH;
    SU_strcpy(buf+pos,((FM_PQueue)Ptr->Data)->Host->Comment,len);
    pos += len;
    /* Flush Host IP */
    *(FFSS_Field *)(buf+pos) = FFSS_IP_TYPE;
    pos += sizeof(FFSS_Field);
    FFSS_PackIP(buf+pos,((FM_PQueue)Ptr->Data)->Host->IP,FFSS_IP_TYPE);
    pos += FFSS_IP_FIELD_SIZE;
    Ptr = Ptr->Next;
  }

  /* Emptying Queue */
  SU_FreeListElem(Queue);

  *size_out = pos;
  return buf;
}


/* Returns a buffer to be sent then freed */
char *FM_BuildServerListing(const char Domain[],const char OS[],long int *size_out)
{
  char *buf;
  SU_PList Ptr,Ptr2;
  long int len,pos,i,j,pos_hst;

  Ptr = FM_Domains;
  len = 0;
  j = 0;
  while(Ptr != NULL)
  {
    if(SU_nocasestrwcmp(((FM_PDomain)Ptr->Data)->Name,Domain))
    {
      len += sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_2 + FFSS_MAX_DOMAIN_LENGTH+1;
      i = SU_ListCount(((FM_PDomain)Ptr->Data)->Hosts);
      len += i * (sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_3 + FFSS_IP_FIELD_SIZE + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1);
      j++;
    }
    Ptr = Ptr->Next;
  }

  buf = (char *) malloc(len);
  pos = 0;

  /* Flush number of domains */
  *(FFSS_Field *)(buf+pos) = j;
  pos += sizeof(FFSS_Field);

  /* Flush Domains */
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    if(SU_nocasestrwcmp(((FM_PDomain)Ptr->Data)->Name,Domain))
    {
      /* Flush Domain */
      len = strlen(((FM_PDomain)Ptr->Data)->Name)+1;
      if(len > FFSS_MAX_DOMAIN_LENGTH)
        len = FFSS_MAX_DOMAIN_LENGTH;
      SU_strcpy(buf+pos,((FM_PDomain)Ptr->Data)->Name,len);
      pos += len;

      pos_hst = pos; /* Remember pos of NbHosts */
      pos += sizeof(FFSS_Field);
      i = 0;

      Ptr2 = ((FM_PDomain)Ptr->Data)->Hosts;
      while(Ptr2 != NULL)
      {
        if(SU_nocasestrwcmp(((FM_PHost)Ptr2->Data)->OS,OS))
        {
          /* Flush State */
          *(FFSS_Field *)(buf+pos) = ((FM_PHost)Ptr2->Data)->State;
          pos += sizeof(FFSS_Field);

          /* Flush Name */
          len = strlen(((FM_PHost)Ptr2->Data)->Name)+1;
          if(len > FFSS_MAX_SERVERNAME_LENGTH)
            len = FFSS_MAX_SERVERNAME_LENGTH;
          SU_strcpy(buf+pos,((FM_PHost)Ptr2->Data)->Name,len);
          pos += len;

          /* Flush OS */
          len = strlen(((FM_PHost)Ptr2->Data)->OS)+1;
          if(len > FFSS_MAX_SERVEROS_LENGTH)
            len = FFSS_MAX_SERVEROS_LENGTH;
          SU_strcpy(buf+pos,((FM_PHost)Ptr2->Data)->OS,len);
          pos += len;

          /* Flush Comment */
          len = strlen(((FM_PHost)Ptr2->Data)->Comment)+1;
          if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
            len = FFSS_MAX_SERVERCOMMENT_LENGTH;
          SU_strcpy(buf+pos,((FM_PHost)Ptr2->Data)->Comment,len);
          pos += len;

          /* Flush Host IP */
          *(FFSS_Field *)(buf+pos) = FFSS_IP_TYPE;
          pos += sizeof(FFSS_Field);
          FFSS_PackIP(buf+pos,((FM_PHost)Ptr2->Data)->IP,FFSS_IP_TYPE);
          pos += FFSS_IP_FIELD_SIZE;

          i++;
        }

        Ptr2 = Ptr2->Next;
      }
      /* Flush number of hosts in the domain */
      *(FFSS_Field *)(buf+pos_hst) = i;
    }
    Ptr = Ptr->Next;
  }

  *size_out = pos;
  return buf;
}
