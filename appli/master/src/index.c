/* TO DO
 - Bloquer (avec sem) l'acces a la recherche lors d'un update et vice versa
 - Ajouter des timers pour l insert/delete
*/

#include "index.h"
#include <assert.h>
#include <ctype.h>

/* Used for ASCII -> internal transcoding */
char HashLUT[256];

/* Hash table */
FM_PSTNode *FMI_HashTable;

/* Host controler */
FM_TControler FM_Controler;

/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     CREATE FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

/*
 * FMI_CreateHashTable
 *   Creates the hash table
 */
void FMI_CreateHashTable()
{
  FMI_HashTable = (FM_PSTNode *) malloc(sizeof(FM_PSTNode *)*HASHTABLE_SIZE);
  memset(FMI_HashTable,0,sizeof(FM_PSTNode *)*HASHTABLE_SIZE);
}

/* Initialize HashLUT for ASCII->internal transcoding
 * 0 = A, 25=Z, 26 = {0,1,2,3,4,5,6,7,8,9 }, 27 = invalid
 */
void FMI_InitHashLUT()
{
  short i;

  for (i=0 ; i< 256 ; i++) HashLUT[i] = INVALID_CHAR;
  for (i= 48 ; i<58 ; i++) HashLUT[i] = NUMBER_CHAR;
  for (i= 65 ; i< 91 ; i++) HashLUT[i] = i - 65; /* upcase */
  for (i= 97 ; i< 123 ; i++) HashLUT[i] = i - 97; /*lowcase */
  /* Special cases ... */
  HashLUT[129] = 19; /* ü */
  HashLUT[130] = 4; /* é */
  HashLUT[131] = 0; /* â */
  HashLUT[132] = 0; /* ä */
  HashLUT[133] = 0; /* à */
  HashLUT[134] = 0; /* a norsk */
  HashLUT[135] = 2; /* ç */
  HashLUT[136] = 4; /* ê */
  HashLUT[137] = 4; /* ë */
  HashLUT[138] = 4; /* è */
  HashLUT[139] = 7; /* î */
  HashLUT[140] = 7; /* ï */
  HashLUT[141] = 7; /* ì */
  HashLUT[142] = 0; /*  */
  HashLUT[143] = 0; /*  */
  HashLUT[144] = 4; /*  */
  HashLUT[147] = 13; /* ô */
  HashLUT[148] = 13; /* ö */
  HashLUT[149] = 13; /* ò */
  HashLUT[150] = 19; /* û */
  HashLUT[151] = 19; /* ù */
  HashLUT[153] = 13;
  HashLUT[154] = 19;
  HashLUT[160] = 0;
  HashLUT[161] = 7;
  HashLUT[162] = 13;
  HashLUT[163] = 19;
  HashLUT[164] = 12; /* ñ */
  HashLUT[165] = 12; /* Ñ */
}

/*
 * FMI_IndexInit
 *   Initialize the indexing engine
 *   Must be called before any indexing function
 */
void FMI_IndexInit()
{
  FMI_CreateHashTable();
  FMI_InitHashLUT();
  memset(&FM_Controler,0,sizeof(FM_Controler));
}

/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     INSERT FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

/*
 * FMI_HashKeyWord
 *   Hashs a keyword, returning the index in the hash table
 *   Assumes strlen(Keyword) == 4
 */
int FMI_HashKeyWord(const unsigned char *KeyWord)  /* <-- KeyWord to hash */
{
  return ( (( HashLUT[KeyWord[0]]   * HASHTABLE_LENGTH  +
              HashLUT[KeyWord[1]] ) * HASHTABLE_LENGTH +
              HashLUT[KeyWord[2]] ) * HASHTABLE_LENGTH +
              HashLUT[KeyWord[3]] );
}

/*
 * FMI_AddSTNodeToSTNode
 *   Adds a new SuffixTreeNode to a SuffixTreeNode for Letter, and returns the new Node
 *   If Letter already exists, returns existing STNode
 */
FM_PSTNode FMI_AddSTNodeToSTNode(FM_PSTNode Node,  /* <-- Node which be reallocated */
                                 char Letter)      /* <-- Letter to add             */
{
  int i,insert_pos,move_pos;

  if(Node->NbNodes == 0)
  {
    Node->STNodes = (FM_TSTNode *) malloc(sizeof(FM_TSTNode));
    memset(&Node->STNodes[Node->NbNodes],0,sizeof(FM_TSTNode));
    Node->STNodes[Node->NbNodes].Letter = Letter;
    Node->NbNodes++;
    return &Node->STNodes[Node->NbNodes-1];
  }
  else
  {
    insert_pos = -1;
    move_pos = -1;
    for(i=0;i<Node->NbNodes;i++)
    {
      if(Node->STNodes[i].Letter == Letter)
      {
        return &Node->STNodes[i];
      }
      else if((Node->STNodes[i].Letter > Letter) && (insert_pos == -1))
        insert_pos = i;
      else if((Node->STNodes[i].Letter == UNUSED_CHAR) && (insert_pos != -1))
        move_pos = i;
    }
    if((insert_pos > 0) && (Node->STNodes[insert_pos-1].Letter == UNUSED_CHAR)) /* Insert in unused pos */
      insert_pos--;
    else if((insert_pos == -1) && (Node->STNodes[Node->NbNodes-1].Letter == UNUSED_CHAR)) /* Insert in last pos (unused) */
    {
      i = Node->NbNodes-2;
      while(i >= 0)
      {
        if(Node->STNodes[i].Letter != UNUSED_CHAR)
          break;
        i--;
      }
      insert_pos = i + 1;
    }
    else
    {
      if(move_pos == -1) /* Must realloc... no unused pos */
      {
        Node->STNodes = (FM_TSTNode *) realloc(Node->STNodes,(Node->NbNodes+1)*sizeof(FM_TSTNode));
        move_pos = Node->NbNodes;
        Node->NbNodes++;
      }
      if(insert_pos == -1) /* means move_pos WAS -1 too */
        insert_pos = Node->NbNodes-1;
      else
        memmove(&Node->STNodes[insert_pos+1],&Node->STNodes[insert_pos],(move_pos-insert_pos)*sizeof(FM_TSTNode));
      memset(&Node->STNodes[insert_pos],0,sizeof(FM_TSTNode));
    }
    Node->STNodes[insert_pos].Letter = Letter;
    return &Node->STNodes[insert_pos];
  }
}


/*
 * FMI_GetSTLeafFromSTNode
 *   Gets the SuffixTreeLeaf of matching NumHost from a SuffixTreeNode and returns it.
 *   Returns NULL if not found.
 */
FM_PSTLeaf FMI_GetSTLeafFromSTNode(FM_PSTNode Node,  /* <-- Node to search Host in */
                                   int NumHost)      /* <-- Host index             */
{
  /* Cherche dicothomique */
  return NULL;
}

/*
 * FMI_AddFileToSTLeaf
 *   Adds a new NumFile to a SuffixTreeLeaf
 */
void FMI_AddFileToSTLeaf(FM_PSTLeaf Leaf,  /* <-- Leaf which be reallocated */
                         int NumFile)      /* <-- File index                */
{
  int i,insert_pos;

  if(Leaf->NbFiles == 0)
  {
    Leaf->NumFiles = (int *) malloc(sizeof(int));
    Leaf->NumFiles[0] = NumFile;
    Leaf->NbFiles++;
  }
  else
  {
    insert_pos = -1;
    for(i=0;i<Leaf->NbFiles;i++)
    {
      if(Leaf->NumFiles[i] == NumFile) /* Same file already in index */
        return;
      if(Leaf->NumFiles[i] > NumFile)
      {
        insert_pos = i;
        break;
      }
    }
    Leaf->NumFiles = (int *) realloc(Leaf->NumFiles,(Leaf->NbFiles+1)*sizeof(int));
    if(insert_pos == -1)
      insert_pos = Leaf->NbFiles;
    else
      memmove(&Leaf->NumFiles[insert_pos+1],&Leaf->NumFiles[insert_pos],(Leaf->NbFiles-insert_pos)*sizeof(int));
    Leaf->NbFiles++;
    Leaf->NumFiles[insert_pos] = NumFile;
  }
}

/*
 * FMI_AddSTLeafToSTNode
 *   Adds a new SuffixTreeLeaf to a SuffixTreeNode
 */
void FMI_AddSTLeafToSTNode(FM_PSTNode Node,  /* <-- Node which be reallocated */
                           int NumHost,      /* <-- Host index                */
                           int NumFile)      /* <-- File index                */
{
  int i,insert_pos,move_pos;
  bool exists;

  if(Node->NbLeafs == 0)
  {
    Node->STLeafs = (FM_TSTLeaf *) malloc(sizeof(FM_TSTLeaf));
    memset(&Node->STLeafs[Node->NbLeafs],0,sizeof(FM_TSTLeaf));
    Node->STLeafs[Node->NbLeafs].NumHost = NumHost;
    FMI_AddFileToSTLeaf(&Node->STLeafs[Node->NbLeafs],NumFile);
    Node->NbLeafs++;
  }
  else
  {
    insert_pos = -1;
    move_pos = -1;
    exists = false;
    for(i=0;i<Node->NbLeafs;i++)
    {
      if(Node->STLeafs[i].NumHost == NumHost)
      {
        insert_pos = i;
        exists = true;
        break;
      }
      else if((Node->STLeafs[i].NumHost > NumHost) && (insert_pos == -1))
        insert_pos = i;
      else if((Node->STLeafs[i].NumHost == -1) && (insert_pos != -1))
        move_pos = i;
    }
    if(exists)
    {
      FMI_AddFileToSTLeaf(&Node->STLeafs[insert_pos],NumFile);
    }
    else
    {
      if((insert_pos > 0) && (Node->STLeafs[insert_pos].NumHost == -1)) /* Insert in unused pos */
        insert_pos--;
      else if((insert_pos == -1) && (Node->STLeafs[Node->NbLeafs-1].NumHost == -1)) /* Insert in last pos (unused) */
      {
        i = Node->NbLeafs-2;
        while(i >= 0)
        {
          if(Node->STLeafs[Node->NbLeafs-1].NumHost == -1)
            break;
          i--;
        }
        insert_pos = i + 1;
      }
      else
      {
        if(move_pos == -1) /* Must realloc... no unused pos */
        {
          Node->STLeafs = (FM_TSTLeaf *) realloc(Node->STLeafs,(Node->NbLeafs+1)*sizeof(FM_TSTLeaf));
          move_pos = Node->NbLeafs;
          Node->NbLeafs++;
        }
        if(insert_pos == -1) /* means move_pos WAS -1 too */
          insert_pos = Node->NbLeafs-1;
        else
          memmove(&Node->STLeafs[insert_pos+1],&Node->STLeafs[insert_pos],(move_pos-insert_pos)*sizeof(FM_TSTLeaf));
        memset(&Node->STLeafs[insert_pos],0,sizeof(FM_TSTLeaf));
      }
      Node->STLeafs[insert_pos].NumHost = NumHost;
      FMI_AddFileToSTLeaf(&Node->STLeafs[insert_pos],NumFile);
    }
  }
}

void FMI_InsertKeyWordInSuffixTree_Rec(FM_PSTNode Node,   /* <-- Current Node  */
                                       const char *Word,  /* <-- Word to parse */
                                       int NumHost,       /* <-- Host index    */
                                       int NumFile)       /* <-- File index    */
{
  FM_PSTNode NewNode;

  if(Word[0] == 0) /* Add leaf */
  {
    FMI_AddSTLeafToSTNode(Node,NumHost,NumFile);
  }
  else /* Add node */
  {
    NewNode = FMI_AddSTNodeToSTNode(Node,HashLUT[(unsigned char)Word[0]]);
    FMI_InsertKeyWordInSuffixTree_Rec(NewNode,Word+1,NumHost,NumFile);
  }
}

/*
 * FMI_InsertKeyWord
 *   Inserts a key word in the hash table, then in the suffix tree
 *   Assumes strlen(Word) >= 4
 */
void FMI_InsertKeyWordInSuffixTree(const char *Word,  /* <--  Key word to insert                         */
                                   int NumHost,       /* <--  Position of the host in hosts table        */
                                   int NumFile)       /* <--  Position of the file in host's files table */
{
  char KW[1024];
  char HW[5];
  int len,i,hash_pos;
  FM_PSTNode Node;

  assert(strlen(Word) >= 4);
  SU_strcpy(KW,Word,sizeof(KW));
  len = strlen(KW);
  for(i=0;i<=(len-4);i++)
  {
    SU_strcpy(HW,KW+i,sizeof(HW));
    hash_pos = FMI_HashKeyWord(HW);
    Node = FMI_HashTable[hash_pos];
    if(Node == NULL)
    {
      Node = (FM_PSTNode) malloc(sizeof(FM_TSTNode));
      memset(Node,0,sizeof(FM_TSTNode));
      Node->Letter = ROOT_CHAR;
      FMI_HashTable[hash_pos] = Node;
    }
    Node->Letter = ROOT_CHAR;
    FMI_InsertKeyWordInSuffixTree_Rec(Node,KW+i+4,NumHost,NumFile);
  }
}

/*
 * FMI_InsertHostInIndex
 *   Inserts a host's index in the global index
 *   (NOT INCREMENTAL)
 */
void FMI_InsertHostInIndex(FM_PFTControler Host,  /* <-- Host controler structure */
                           int NumHost)           /* <-- Number of this host      */
{
  int i,j,s,e,len;
  char buf[1024];

#ifdef DEBUG
  printf("Indexing engine : Inserting host '%s' (%d nodes)\n",Host->Name,Host->NbNodes);
#endif /* DEBUG */
  context;
  for(i=0;i<Host->NbNodes;i++)
  {
    SU_strcpy(buf,Host->FileTree+Host->FTNodes[i].Pos,sizeof(buf));
#ifdef DEBUG
    //printf("Indexing engine : Inserting node '%s' (%d)\n",buf,i);
#endif /* DEBUG */
    for(j=0;j<strlen(buf);j++)
      buf[j] = SU_toupper(buf[j]);
    len = strlen(buf);
    s = 0;
    while(s <= (len-4))
    {
      e = 0;
      while((s+e) < len)
      {
        if(!isalnum(buf[s+e]))
        {
          buf[s+e] = 0;
          break;
        }
        e++;
      }
      if(e >= 4)
      {
        FMI_InsertKeyWordInSuffixTree(buf+s,NumHost,i);
      }
      s += e + 1;
    }
  }
#ifdef DEBUG
  printf("Indexing engine : Inserting of '%s' complete\n",Host->Name);
#endif /* DEBUG */
}


/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                      FREE FUNCTIONS                      */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

/*
 * FMI_FreeFTControler
 *   Frees a FTControler
 */
void FMI_FreeFTControler(FM_PFTControler Host)  /* <-- FTControler structure to free */
{
  if(Host->FileTree != NULL)
    free(Host->FileTree);
  if(Host->NbNodes != 0)
    free(Host->FTNodes);
  if(Host->IP != NULL)
    free(Host->IP);
  free(Host->Name);
  free(Host);
}

void FMI_FreeSTLeaf(FM_PSTLeaf Leaf)
{
  if(Leaf->NbFiles != 0)
    free(Leaf->NumFiles);
  /* Do NOT free Leaf, because it comes from a table */
}

void FMI_FreeSTNode(FM_PSTNode Node)
{
  int i;

  if(Node->NbNodes != 0)
  {
    for(i=0;i<Node->NbNodes;i++)
      FMI_FreeSTNode(&Node->STNodes[i]);
    free(Node->STNodes);
    Node->NbNodes = 0;
  }
  if(Node->NbLeafs != 0)
  {
    for(i=0;i<Node->NbLeafs;i++)
      FMI_FreeSTLeaf(&Node->STLeafs[i]);
    free(Node->STLeafs);
    Node->NbLeafs = 0;
  }
  /* Do NOT free Node, because it comes from a table */
}


/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     REMOVE FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

bool FMI_RemoveHostFromSuffixTree_Rec(FM_PSTNode Node,
                                      int Host)
{
  int i;
  bool remove=true,rec_remove=true;

  if(Node->Letter != UNUSED_CHAR)
  {
    for(i=0;i<Node->NbNodes;i++)
    {
      context;
      if(FMI_RemoveHostFromSuffixTree_Rec(&Node->STNodes[i],Host) == false)
        rec_remove = false;
    }
    for(i=0;i<Node->NbLeafs;i++)
    {
      if(Node->STLeafs[i].NumHost == Host)
      {
        context;
        Node->STLeafs[i].NumHost = -1;
        if(Node->STLeafs[i].NbFiles != 0)
        {
          Node->STLeafs[i].NbFiles = 0;
          free(Node->STLeafs[i].NumFiles);
          Node->STLeafs[i].NumFiles = NULL;
        }
      }
      else if(Node->STLeafs[i].NumHost != -1)
        remove = false;
    }
    context;
    remove = remove & rec_remove;
    if(remove)
      Node->Letter = UNUSED_CHAR;
  }
  return remove;
}

void FMI_RemoveHostFromSuffixTree(int Host)
{
  int i;
  FM_PSTNode Node;

  context;
  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(FMI_HashTable[i] != NULL)
    {
      Node = FMI_HashTable[i];
      FMI_RemoveHostFromSuffixTree_Rec(Node,Host);
    }
  }
}

void FMI_ShiftSTNodes(FM_PSTNode Node,int pos)
{
  context;
  Node->NbNodes--;
  if(Node->NbNodes == 0)
    free(Node->STNodes);
  else
  {
    if(pos != Node->NbNodes) /* If pos is not the last one, we must shift */
      memmove(&Node->STNodes[pos],&Node->STNodes[pos+1],(Node->NbNodes-pos)*sizeof(FM_TSTNode));
    Node->STNodes = (FM_TSTNode *) realloc(Node->STNodes,Node->NbNodes*sizeof(FM_TSTNode));
  }
}

void FMI_ShiftSTLeafs(FM_PSTNode Node,int pos)
{
  context;
  Node->NbLeafs--;
  if(Node->NbLeafs == 0)
    free(Node->STLeafs);
  else
  {
    if(pos != Node->NbLeafs) /* If pos is not the last one, we must shift */
      memmove(&Node->STLeafs[pos],&Node->STLeafs[pos+1],(Node->NbLeafs-pos)*sizeof(FM_TSTLeaf));
    Node->STLeafs = (FM_TSTLeaf *) realloc(Node->STLeafs,Node->NbLeafs*sizeof(FM_TSTLeaf));
  }
}

bool FMI_GarbageSuffixTree_Rec(FM_PSTNode Node,bool purge)
{
  int i;
  bool remove=true,rec_remove=true;

  if(purge) /* Remove now */
  {
    FMI_FreeSTNode(Node); /* Recursively free Node, except Node itself */
    /* remove == true */
  }
  else /* Test what to remove */
  {
    i = 0;
    while(i<Node->NbNodes)
    {
      if(Node->STNodes[i].Letter == UNUSED_CHAR) /* Unused... force to remove all sub nodes */
      {
        FMI_GarbageSuffixTree_Rec(&Node->STNodes[i],true); /* Recursively free STNodes[i], except STNodes[i] itself */
        FMI_ShiftSTNodes(Node,i); /* Remove STNodes[i] from table */
      }
      else
      {
        if(FMI_GarbageSuffixTree_Rec(&Node->STNodes[i],false) == false) /* Some used node still here... do not remove */
        {
          rec_remove = false;
          i++; /* Go to next Node */
        }
        else
          FMI_ShiftSTNodes(Node,i); /* Remove STNodes[i] from table */
      }
    }
    i = 0;
    while(i<Node->NbLeafs)
    {
      if(Node->STLeafs[i].NumHost == -1)
      {
        FMI_FreeSTLeaf(&Node->STLeafs[i]);
        FMI_ShiftSTLeafs(Node,i);
      }
      else
      {
        remove = false;
        i++; /* Go to next Leaf */
      }
    }
    remove = remove & rec_remove;
    if(remove)
      FMI_FreeSTNode(Node);
  }
  return remove; /* Wether to remove this Node or not */
}

void FMI_GarbageCollector(void)
{
  int i;
  FM_PSTNode Node;

  SU_SEM_WAIT(FM_MySem5);
  FFSS_PrintDebug(4,"Starting garbage collector...\n");
  context;
  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(FMI_HashTable[i] != NULL)
    {
      Node = FMI_HashTable[i];
      if(FMI_GarbageSuffixTree_Rec(Node,false)) /* Remove full node */
      {
        free(Node);
        FMI_HashTable[i] = NULL;
      }
    }
  }
  FFSS_PrintDebug(4,"Garbage collector completed\n");
  SU_SEM_POST(FM_MySem5);
}


/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     SEARCH FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

FM_PSTNode FMI_SearchNodeFromLetter(FM_PSTNode Node,char Letter)
{
  int i;

  for(i=0;i<Node->NbNodes;i++) /* TO DO : Dichotomique */
  {
    if(Node->STNodes[i].Letter == HashLUT[(unsigned char)Letter])
      return &Node->STNodes[i];
  }
  return NULL;
}

FM_PSTNode FMI_SearchKeyWordInSuffixTree(FM_PSTNode Node,   /* <-- Current Node  */
                                         const char *Word)  /* <-- Word to parse */
{
  FM_PSTNode Curr,Next;

  Curr = Node;
  while(*Word != 0)
  {
    Next = FMI_SearchNodeFromLetter(Curr,*Word);
    if(Next == NULL)
      return NULL;
    Word++;
    Curr = Next;
  }
  return Curr;
}

/* FMI_SearchKey
 *   Returns a FM_PSTNode matching the search key
 *   Assumes strlen(SearchKey) >= 4
 */
FM_PSTNode FMI_SearchKey(const char *SearchKey) /* <-- Search key to search for */
{
  char HW[5];
  int hash_pos;
  FM_PSTNode Node;

  assert(strlen(SearchKey) >= 4);
  SU_strcpy(HW,SearchKey,sizeof(HW));
  hash_pos = FMI_HashKeyWord(HW);
  Node = FMI_HashTable[hash_pos];
  if(Node != NULL)
    Node = FMI_SearchKeyWordInSuffixTree(Node,SearchKey+4);

  return Node;
}


/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                  LOAD/STORE  FUNCTIONS                   */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

void FMI_StoreSuffixTree_rec(FILE *fp,FM_PSTNode Root)
{
  int i;

  fwrite(&Root->Letter,sizeof(Root->Letter),1,fp);
  fwrite(&Root->NbLeafs,sizeof(Root->NbLeafs),1,fp);
  for(i=0;i<Root->NbLeafs;i++)
  {
    fwrite(&Root->STLeafs[i].NumHost,sizeof(Root->STLeafs[i].NumHost),1,fp);
    fwrite(&Root->STLeafs[i].NbFiles,sizeof(Root->STLeafs[i].NbFiles),1,fp);
    if(Root->STLeafs[i].NbFiles != 0)
      fwrite(Root->STLeafs[i].NumFiles,Root->STLeafs[i].NbFiles*sizeof(int),1,fp);
  }
  fwrite(&Root->NbNodes,sizeof(Root->NbNodes),1,fp);
  for(i=0;i<Root->NbNodes;i++)
    FMI_StoreSuffixTree_rec(fp,&Root->STNodes[i]);
}

void FMI_StoreSuffixTree(FILE *fp)
{
  int i;
  FM_PSTNode Node;
  bool used;

  context;
  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(FMI_HashTable[i] != NULL)
    {
      used = true;
      fwrite(&used,sizeof(used),1,fp);
      Node = FMI_HashTable[i];
      FMI_StoreSuffixTree_rec(fp,Node);
    }
    else
    {
      used = false;
      fwrite(&used,sizeof(used),1,fp);
    }
  }
}

bool FMI_StoreFTNodes(FILE *fp,FM_PFTNode Node)
{
  fwrite(&Node->Pos,sizeof(Node->Pos),1,fp);
  fwrite(&Node->Father,sizeof(Node->Father),1,fp);
  fwrite(&Node->Last,sizeof(Node->Last),1,fp);
  fwrite(&Node->Tags,sizeof(Node->Tags),1,fp);
  return true;
}

bool FMI_StoreFileTrees(FILE *fp)
{
  int i,j;
  long int Length;

  context;
  fwrite(&FM_Controler.NbHosts,sizeof(FM_Controler.NbHosts),1,fp);
  for(i=0;i<FM_Controler.NbHosts;i++)
  {
    fwrite(&FM_Controler.Hosts[i]->Samba,sizeof(FM_Controler.Hosts[i]->Samba),1,fp);
    Length = strlen(FM_Controler.Hosts[i]->Name) + 1;
    fwrite(&Length,sizeof(Length),1,fp);
    fwrite(FM_Controler.Hosts[i]->Name,Length,1,fp);
    Length = strlen(FM_Controler.Hosts[i]->IP) + 1;
    fwrite(&Length,sizeof(Length),1,fp);
    fwrite(FM_Controler.Hosts[i]->IP,Length,1,fp);
    fwrite(&FM_Controler.Hosts[i]->FileTreeLength,sizeof(FM_Controler.Hosts[i]->FileTreeLength),1,fp);
    fwrite(FM_Controler.Hosts[i]->FileTree,FM_Controler.Hosts[i]->FileTreeLength,1,fp);
    fwrite(&FM_Controler.Hosts[i]->NbNodes,sizeof(FM_Controler.Hosts[i]->NbNodes),1,fp);
    for(j=0;j<FM_Controler.Hosts[i]->NbNodes;j++)
      FMI_StoreFTNodes(fp,&FM_Controler.Hosts[i]->FTNodes[j]);
  }
  return true;
}

/*
 * FMI_SaveIndex
 *   Saves the full index to a file
 */
bool FMI_SaveIndex(const char FileName[]) /* <-- Name of the file to save index to */
{
  FILE *fp;
  FFSS_Field Version;

  FFSS_PrintDebug(1,"Dumping index to disk... (%s)\n",FileName);
  fp = fopen(FileName,"wb");
  if(fp == NULL)
    return false;

  context;
  SU_SEM_WAIT(FM_MySem5);
  Version = FM_INDEX_VERSION;
  fwrite(&Version,sizeof(Version),1,fp);
  FMI_StoreFileTrees(fp);
  FMI_StoreSuffixTree(fp);
  SU_SEM_POST(FM_MySem5);

  FFSS_PrintDebug(1,"Dump complete.\n");
  fclose(fp);
  return true;
}


bool FMI_LoadSuffixTree_rec(FILE *fp,FM_PSTNode Root)
{
  int i;

  fread(&Root->Letter,sizeof(Root->Letter),1,fp);
  fread(&Root->NbLeafs,sizeof(Root->NbLeafs),1,fp);
  if(Root->NbLeafs != 0)
  {
    Root->STLeafs = (FM_TSTLeaf *) malloc(Root->NbLeafs*sizeof(FM_TSTLeaf));
    if(Root->STLeafs == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Load Suffix Tree : Not enough memory\n");
      return false;
    }
    for(i=0;i<Root->NbLeafs;i++)
    {
      fread(&Root->STLeafs[i].NumHost,sizeof(Root->STLeafs[i].NumHost),1,fp);
      fread(&Root->STLeafs[i].NbFiles,sizeof(Root->STLeafs[i].NbFiles),1,fp);
      if(Root->STLeafs[i].NbFiles != 0)
      {
        Root->STLeafs[i].NumFiles = (int *) malloc(Root->STLeafs[i].NbFiles*sizeof(int));
        if(Root->STLeafs[i].NumFiles == NULL)
        {
          FFSS_PrintSyslog(LOG_ERR,"Load Suffix Tree : Not enough memory\n");
          return false;
        }
        fread(Root->STLeafs[i].NumFiles,Root->STLeafs[i].NbFiles*sizeof(int),1,fp);
      }
    }
  }
  fread(&Root->NbNodes,sizeof(Root->NbNodes),1,fp);
  if(Root->NbNodes != 0)
  {
    Root->STNodes = (FM_TSTNode *) malloc(Root->NbNodes*sizeof(FM_TSTNode));
    if(Root->STNodes == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Load Suffix Tree : Not enough memory\n");
      return false;
    }
    for(i=0;i<Root->NbNodes;i++)
    {
      if(FMI_LoadSuffixTree_rec(fp,&Root->STNodes[i]) == false)
        return false;
    }
  }
  return true;
}

bool FMI_LoadSuffixTree(FILE *fp)
{
  int i;
  FM_PSTNode Root;
  bool used;

  context;
  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(fread(&used,sizeof(used),1,fp) == 0)
    {
      FFSS_PrintSyslog(LOG_ERR,"Load Suffix Tree : Corrupted file : Not enough elements in hash table : %d\n",i);
      return false;
    }
    if(used)
    {
      Root = (FM_PSTNode) malloc(sizeof(FM_TSTNode));
      if(Root == NULL)
      {
        FFSS_PrintSyslog(LOG_ERR,"Load Suffix Tree : Not enough memory\n");
        return false;
      }
      memset(Root,0,sizeof(FM_TSTNode));
      if(FMI_LoadSuffixTree_rec(fp,Root) == false)
        return false;
      FMI_HashTable[i] = Root;
    }
  }
  return true;
}

bool FMI_LoadFTNodes(FILE *fp,FM_PFTNode Node)
{
  fread(&Node->Pos,sizeof(Node->Pos),1,fp);
  fread(&Node->Father,sizeof(Node->Father),1,fp);
  fread(&Node->Last,sizeof(Node->Last),1,fp);
  fread(&Node->Tags,sizeof(Node->Tags),1,fp);
  return true;
}

bool FMI_LoadFileTrees(FILE *fp)
{
  int i,j;
  long int Length;

  context;
  fread(&FM_Controler.NbHosts,sizeof(FM_Controler.NbHosts),1,fp);
  if(FM_Controler.NbHosts != 0)
  {
    FM_Controler.Hosts = (FM_PFTControler *) malloc(sizeof(FM_PFTControler)*FM_Controler.NbHosts);
    if(FM_Controler.Hosts == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
      return false;
    }
    memset(FM_Controler.Hosts,0,sizeof(FM_PFTControler)*FM_Controler.NbHosts);
    for(i=0;i<FM_Controler.NbHosts;i++)
    {
      FM_Controler.Hosts[i] = (FM_PFTControler) malloc(sizeof(FM_TFTControler));
      if(FM_Controler.Hosts[i] == NULL)
      {
        FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
        return false;
      }
      memset(FM_Controler.Hosts[i],0,sizeof(FM_TFTControler));
      fread(&FM_Controler.Hosts[i]->Samba,sizeof(FM_Controler.Hosts[i]->Samba),1,fp);
      FM_Controler.Hosts[i]->State = FFSS_STATE_OFF;
      fread(&Length,sizeof(Length),1,fp);
      FM_Controler.Hosts[i]->Name = (char *) malloc(Length);
      if(FM_Controler.Hosts[i]->Name == NULL)
      {
        FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
        return false;
      }
      fread(FM_Controler.Hosts[i]->Name,Length,1,fp);
      fread(&Length,sizeof(Length),1,fp);
      FM_Controler.Hosts[i]->IP = (char *) malloc(Length);
      if(FM_Controler.Hosts[i]->IP == NULL)
      {
        FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
        return false;
      }
      fread(FM_Controler.Hosts[i]->IP,Length,1,fp);
      fread(&FM_Controler.Hosts[i]->FileTreeLength,sizeof(FM_Controler.Hosts[i]->FileTreeLength),1,fp);
      FM_Controler.Hosts[i]->FileTree = (char *) malloc(FM_Controler.Hosts[i]->FileTreeLength);
      if(FM_Controler.Hosts[i]->FileTree == NULL)
      {
        FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
        return false;
      }
      fread(FM_Controler.Hosts[i]->FileTree,FM_Controler.Hosts[i]->FileTreeLength,1,fp);
      fread(&FM_Controler.Hosts[i]->NbNodes,sizeof(FM_Controler.Hosts[i]->NbNodes),1,fp);
      if(FM_Controler.Hosts[i]->NbNodes != 0)
      {
        FM_Controler.Hosts[i]->FTNodes = (FM_TFTNode *) malloc(sizeof(FM_TFTNode)*FM_Controler.Hosts[i]->NbNodes);
        if(FM_Controler.Hosts[i]->FTNodes == NULL)
        {
          FFSS_PrintSyslog(LOG_ERR,"Load File Trees : Not enough memory\n");
          return false;
        }
        memset(FM_Controler.Hosts[i]->FTNodes,0,sizeof(FM_TFTNode)*FM_Controler.Hosts[i]->NbNodes);
        for(j=0;j<FM_Controler.Hosts[i]->NbNodes;j++)
          FMI_LoadFTNodes(fp,&FM_Controler.Hosts[i]->FTNodes[j]);
      }
    }
  }
  return true;
}

/*
 * FMI_LoadIndex
 *   Loads the full index from a file
 *   Assumes whole index as empty
 */
bool FMI_LoadIndex(const char FileName[]) /* <-- Name of the file to load index from */
{
  FILE *fp;
  bool res;
  FFSS_Field Version;

  context;
  res = false;
  FFSS_PrintDebug(4,"Loading index from disk... (%s)\n",FileName);
  fp = fopen(FileName,"rb");
  if(fp != NULL)
  {
    SU_SEM_WAIT(FM_MySem5);
    fread(&Version,sizeof(Version),1,fp);
    if(Version != FM_INDEX_VERSION)
    {
      FFSS_PrintSyslog(LOG_ERR,"Error loading index file '%s' : Version mismatch (master=0x0%x, index=0x0%x).\nPlease remove it and re-run master\n",FileName,FM_INDEX_VERSION,Version);
      abort();
    }
    res = FMI_LoadFileTrees(fp);
    if(res)
      res = FMI_LoadSuffixTree(fp);
    if(res)
      res = FMI_CheckIndex();
    SU_SEM_POST(FM_MySem5);
    if(!res)
    {
      FFSS_PrintSyslog(LOG_ERR,"Error loading index file '%s'. Please remove it and re-run master\n",FileName);
      abort();
    }
    fclose(fp);
  }
  FFSS_PrintDebug(4,"Load complete : %s\n",res?"Success":"Failed");
  return res;
}

/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     CHECK  FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

/*
 * FMI_CheckFileTree
 *   Checks integrity of File tree
 */
bool FMI_CheckFileTree(FM_PFTControler Host) /* <-- Host to check */
{
  int i,count;
  FM_PFTNode Curr;

  context;
  FFSS_PrintDebug(6,"Checking index integrity of %s\n",Host->Name);
  /* Check FileTree string */
  if(Host->FileTree[Host->FileTreeLength-1] != 0)
  {
    FFSS_PrintSyslog(LOG_WARNING,"CheckFileTree failed for host %s (Invalid FileTree string). DoS attack ?\n",Host->Name);
    return false;
  }

  /* Check Pos/Father/Last values */
  for(i=0;i<Host->NbNodes;i++)
  {
    if(Host->FTNodes[i].Pos != -1)
    {
      if(Host->FTNodes[i].Pos >= Host->FileTreeLength)
      {
        FFSS_PrintSyslog(LOG_WARNING,"CheckFileTree failed for host %s (Offset out of FileTree string). DoS attack ?\n",Host->Name);
        return false;
      }
    }
    if(Host->FTNodes[i].Father != -1)
    {
      if(Host->FTNodes[i].Father >= Host->NbNodes)
      {
        FFSS_PrintSyslog(LOG_WARNING,"CheckFileTree failed for host %s (Father out of Nodes array : %d %d). DoS attack ?\n",Host->Name,Host->FTNodes[i].Father,Host->NbNodes);
        return false;
      }
    }
    if(Host->FTNodes[i].Last >= Host->NbNodes)
    {
      FFSS_PrintSyslog(LOG_WARNING,"CheckFileTree failed for host %s (Last out of Nodes array : %d %d). DoS attack ?\n",Host->Name,Host->FTNodes[i].Last,Host->NbNodes);
      return false;
    }
  }

  /* Check Infinite loop */
  for(i=0;i<Host->NbNodes;i++)
  {
    Curr = &Host->FTNodes[i];
    count = 0;
    while(Curr->Father != -1)
    {
      count++;
      if(count > FM_INDEX_MAX_FATHER_RECURSION)
      {
        FFSS_PrintSyslog(LOG_WARNING,"CheckFileTree failed for host %s (Infinite father-node loop). DoS attack ?\n",Host->Name);
        return false;
      }
      Curr = &Host->FTNodes[Curr->Father];
    }
  }
  return true;
}

/*
 * FMI_CheckIndex
 *   Checks integrity of the whole index
 */
bool FMI_CheckIndex()
{
  int i;

  for(i=0;i<FM_Controler.NbHosts;i++)
    if(FMI_CheckFileTree(FM_Controler.Hosts[i]) == false)
      return false;
  return true;
}


/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */
/*                     DEBUG  FUNCTIONS                     */
/* ******************************************************** */
/* ******************************************************** */
/* ******************************************************** */

#ifdef DEBUG
void FMI_PrintSuffixTree_rec(FILE *fp,FM_PSTNode Node,const char TabString[])
{
  int i;
  char TS[1024];

  if(Node->Letter == NUMBER_CHAR)
    fprintf(fp,"%s0 : ",TabString);
  else if(Node->Letter == INVALID_CHAR)
    fprintf(fp,"%s? : ",TabString);
  else if(Node->Letter == UNUSED_CHAR)
    fprintf(fp,"%s_ : ",TabString);
  else
    fprintf(fp,"%s%c : ",TabString,Node->Letter+65);

  for(i=0;i<Node->NbLeafs;i++)
  {
    if(Node->STLeafs[i].NumHost == -1)
      fprintf(fp,"UNUSED ");
    else
      fprintf(fp,"%s(%d) ",FM_Controler.Hosts[Node->STLeafs[i].NumHost]->Name,Node->STLeafs[i].NbFiles);
  }
  fprintf(fp,"\n");
  snprintf(TS,sizeof(TS),"%s  ",TabString);
  for(i=0;i<Node->NbNodes;i++)
    FMI_PrintSuffixTree_rec(fp,&Node->STNodes[i],TS);
}

void FMI_PrintSuffixTree(FILE *fp,FM_PSTNode Root,int HashPos)
{
  int i;
  char HS[5];
  int next;

  HS[4] = 0;
  next = HashPos;
  for(i=3;i>=0;i--)
  {
    HS[i] = next % HASHTABLE_LENGTH;
    next /= HASHTABLE_LENGTH;
    if(HS[i] == NUMBER_CHAR)
      HS[i] = '0';
    else if(HS[i] == INVALID_CHAR)
      HS[i] = '?';
    else
      HS[i] += 65;
  }
  fprintf(fp,"%s : ",HS);

  for(i=0;i<Root->NbLeafs;i++)
  {
    if(Root->STLeafs[i].NumHost == -1)
      fprintf(fp,"UNUSED ");
    else
      fprintf(fp,"%s(%d) ",FM_Controler.Hosts[Root->STLeafs[i].NumHost]->Name,Root->STLeafs[i].NbFiles);
  }
  fprintf(fp,"\n");
  for(i=0;i<Root->NbNodes;i++)
    FMI_PrintSuffixTree_rec(fp,&Root->STNodes[i],"  ");
}

void FMI_PrintWholeSuffixTree(const char FileName[])
{
  int i;
  FM_PSTNode Node;
  FILE *fp;

  if(FileName == NULL)
    fp = stderr;
  else
    fp = fopen(FileName,"wt");
  if(fp == NULL)
    return;
  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(FMI_HashTable[i] != NULL)
    {
      Node = FMI_HashTable[i];
      FMI_PrintSuffixTree(fp,Node,i);
    }
  }
  if(FileName != NULL)
    fclose(fp);
}


bool FMI_CheckOrderIntegrity_rec(FM_PSTNode Node)
{
  int i,val;
  bool error=false;

  val = -2;
  for(i=0;i<Node->NbLeafs;i++)
  {
    if(Node->STLeafs[i].NumHost != -1)
    {
      if(Node->STLeafs[i].NumHost <= val)
      {
        printf("############### Leafs not ordered ###############\n");
        error = true;
      }
      val = Node->STLeafs[i].NumHost;
    }
  }

  val = -2;
  for(i=0;i<Node->NbNodes;i++)
  {
    if(Node->STNodes[i].Letter != -1)
    {
      if(Node->STNodes[i].Letter <= val)
      {
        printf("############### Nodes not ordered ###############\n");
        error = true;
      }
      val = Node->STNodes[i].Letter;
    }
    error |= FMI_CheckOrderIntegrity_rec(&Node->STNodes[i]);
  }
  return error;
}

bool FMI_CheckOrderIntegrity(void)
{
  int i;
  FM_PSTNode Node;
  bool error=false;

  for(i=0;i<HASHTABLE_SIZE;i++)
  {
    if(FMI_HashTable[i] != NULL)
    {
      Node = FMI_HashTable[i];
      error |= FMI_CheckOrderIntegrity_rec(Node);
    }
  }
  return error;
}

int FMI_GetUnusedHashPos(void)
{
  float nb = 0,size;
  int i;

  for(i=0;i<HASHTABLE_SIZE;i++)
    if(FMI_HashTable[i] == NULL)
      nb++;
  size = HASHTABLE_SIZE;
  return (int)(nb/size*100);
}

int FMI_GetIndexCount(void)
{
  int nb = 0;
  int i;

  for(i=0;i<FM_Controler.NbHosts;i++)
    nb += FM_Controler.Hosts[i]->NbNodes;
  return nb;
}
#endif /* DEBUG */
