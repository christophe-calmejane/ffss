#ifndef __INDEX_H__
#define __INDEX_H__

#include "master.h"

#define FM_INDEX_VERSION 0x01001
#define HASHTABLE_LENGTH 28
#define HASHTABLE_SIZE HASHTABLE_LENGTH*HASHTABLE_LENGTH*HASHTABLE_LENGTH*HASHTABLE_LENGTH
#define NUMBER_CHAR  26
#define INVALID_CHAR 27
#define ROOT_CHAR    99
#define UNUSED_CHAR  -1

typedef struct /* 16 bytes */
{
  int NumHost;                 /* Position of the host in hosts table        */ /* -1 if not used        */
  int NbFiles;                 /* Size of reallocable table NumFiles         */
  int *NumFiles;               /* Position of the file in host's nodes table */ /* Ordered by unique int */
} FM_TSTLeaf, *FM_PSTLeaf;

struct FM_SSTNode;
typedef struct FM_SSTNode /* 16 bytes */
{
  struct FM_SSTNode *STNodes;  /* Table of Nodes                               */ /* Ordered by unique Letter */
  FM_TSTLeaf *STLeafs;         /* Table of leafs                               */ /* Ordered by unique NumHost */
  short int NbNodes;           /* Size of reallocable table STNodes            */
  short int NbLeafs;           /* Size of reallocable table STLeafs            */
  char Letter;                 /* Letter is UNUSED_CHAR if struct not occupied */ /* Hashed letter */
} FM_TSTNode, *FM_PSTNode;

typedef struct /* 16 bytes */
{
  int Pos;                     /* Pos is the offset in 'char *FileTree' for the string of this node        */ /* -1 if not used */
  int Father;                  /* Father is the index in 'FM_TFTNode *FTNodes' for the father of this node */ /* -1 if no father */
  int Last;                    /* Last index in host's nodes table for this directory                      */
  unsigned char Tags;          /* Bit field for tags of file                                               */
} FM_TFTNode, *FM_PFTNode;

typedef struct /* 24 bytes */
{
  char *Name;                  /* The name of the server              */
  char *IP;                    /* IP of the server                    */
  char *FileTree;              /* Huge string with all names of nodes */
  long int FileTreeLength;     /* Length of the huge string           */
  FM_TFTNode *FTNodes;         /* Table of Nodes                      */
  int NbNodes;                 /* Size of reallocable table FTNodes   */
  FFSS_Field State;            /* State of the server                 */
  bool Samba;                  /* Is a samba share                    */
} FM_TFTControler, *FM_PFTControler;

typedef struct /* 8 bytes */
{
  int NbHosts;
  FM_PFTControler *Hosts;
} FM_TControler, *FM_PControler;

extern FM_TControler FM_Controler;


/*
 * FMI_IndexInit
 *   Initialize the indexing engine
 *   Must be called before any indexing function
 */
void FMI_IndexInit();

/*
 * FMI_InsertHostInIndex
 *   Inserts a host's index in the global index
 *   (NOT INCREMENTAL)
 */
void FMI_InsertHostInIndex(FM_PFTControler Host,  /* <-- Host controler structure */
                           int NumHost);          /* <-- Number of this host      */

void FMI_RemoveHostFromSuffixTree(int Host);

/*
 * FMI_FreeFTControler
 *   Frees a FTControler
 */
void FMI_FreeFTControler(FM_PFTControler Host);  /* <-- FTControler structure to free */

/* FMI_SearchKey
 *   Returns a FM_PSTNode matching the search key
 *   Assumes strlen(SearchKey) >= 4
 */
FM_PSTNode FMI_SearchKey(const char *SearchKey); /* <-- Search key to search for */

/*
 * FMI_LoadIndex
 *   Loads the full index from a file
 *   Assumes whole index as empty
 */
bool FMI_LoadIndex(const char FileName[]); /* <-- Name of the file to load index from */
/*
 * FMI_SaveIndex
 *   Saves the full index to a file
 */
bool FMI_SaveIndex(const char FileName[]); /* <-- Name of the file to save index to */

/*
 * FMI_CheckFileTree
 *   Checks integrity of File tree
 */
bool FMI_CheckFileTree(FM_PFTControler Host); /* <-- Host to check */

/*
 * FMI_CheckIndex
 *   Checks integrity of the whole index
 */
bool FMI_CheckIndex();


void FMI_GarbageCollector(void);

#ifdef DEBUG
void FMI_PrintSuffixTree(FILE *fp,FM_PSTNode Root,int HashPos);
void FMI_PrintWholeSuffixTree(const char FileName[]);
bool FMI_CheckOrderIntegrity(void);
int FMI_GetUnusedHashPos(void);
int FMI_GetIndexCount(void);
#endif /* DEBUG */

#endif /* !__INDEX_H__ */
