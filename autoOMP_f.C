#include "rose.h"

#define STR_BUF 256
#define COMP_MSG_LINES 1024*1024

using namespace std;

enum MSG_TYPE
  {
    error, warn, ext, obsol, del, vec, opt, observ, mul, others,
  };

typedef struct
{
  char lang[STR_BUF];
  int msg_id;
  char file_name[STR_BUF];
  int line;  
  int id;
} COMP_MSG;

typedef struct
{
  COMP_MSG *comp_msg[10];
  int msg_cnt[10];
} COMP_LOG;

void read_comp_log( COMP_LOG *comp_log )
{
  int i;
  int index;
  FILE *fp;
  char string_buf[STR_BUF], lang[STR_BUF], type[STR_BUF], file_name[STR_BUF], dummy[STR_BUF];
  int msg_id, line;
  MSG_TYPE msg_type;

  for( i = 0 ; i < 10 ; i ++ ) comp_log->msg_cnt[i] = 0;
  
  if( NULL == (fp = fopen( "compile_log.txt", "r" )))
    {
      cerr<<"file open error\n";
      exit(1);
    }
  while( fgets( string_buf, STR_BUF, fp ))
    {
      if( !strncmp( string_buf, "#", 1 )) continue;
      else if( !strncmp( string_buf, "f90", 3 ) || !strncmp( string_buf, "sxc++", 5 ) || !strncmp( string_buf, "sxcc", 4 ))
	{
	  sscanf( string_buf, "%s %[^(](", lang, type );
	  if( !strncmp( type, "error", 5 ))       msg_type = error;
	  else if( !strncmp( type, "warn", 4 ))   msg_type = warn;
	  else if( !strncmp( type, "ext", 3 ))    msg_type = ext;
	  else if( !strncmp( type, "obsol", 5 ))  msg_type = obsol;
	  else if( !strncmp( type, "del", 3 ))    msg_type = del;
	  else if( !strncmp( type, "vec", 3 ))    msg_type = vec;
	  else if( !strncmp( type, "opt", 3 ))    msg_type = opt;
	  else if( !strncmp( type, "observ", 6 )) msg_type = observ;
	  else if( !strncmp( type, "mul", 3 ))    msg_type = mul;
	  else                                    msg_type = others;
	  comp_log->msg_cnt[msg_type] ++;
	}
    }

  for( i = 0 ; i < 10 ; i ++ )
    {
      comp_log->comp_msg[i] = (COMP_MSG *)malloc( comp_log->msg_cnt[i] * sizeof(COMP_MSG));
      comp_log->msg_cnt[i] = 0;
    }
  
  fseek( fp, 0, SEEK_SET );
  
  while( fgets( string_buf, STR_BUF, fp ))
    {
      if( !strncmp( string_buf, "#", 1 )) continue;
      else if(( !strncmp( string_buf, "f90", 3 )) || ( !strncmp( string_buf, "sxc++", 5 )) || !strncmp( string_buf, "sxcc", 4 ))
  	{
  	  sscanf( string_buf, "%s %[^(](%d %s %[^,], %s %d", lang, type, &msg_id, dummy, file_name, dummy, &line );
	  if( !strncmp( type, "error", 5 ))       msg_type = error;
	  else if( !strncmp( type, "warn", 4 ))   msg_type = warn;
	  else if( !strncmp( type, "ext", 3 ))    msg_type = ext;
	  else if( !strncmp( type, "obsol", 5 ))  msg_type = obsol;
	  else if( !strncmp( type, "del", 3 ))    msg_type = del;
	  else if( !strncmp( type, "vec", 3 ))    msg_type = vec;
	  else if( !strncmp( type, "opt", 3 ))    msg_type = opt;
	  else if( !strncmp( type, "observ", 6 )) msg_type = observ;
	  else if( !strncmp( type, "mul", 3 ))    msg_type = mul;
	  else                                    msg_type = others;

	  index = comp_log->msg_cnt[msg_type];
	  strncpy( comp_log->comp_msg[msg_type][index].lang, lang, STR_BUF );
	  comp_log->comp_msg[msg_type][index].msg_id = msg_id;
	  strncpy( comp_log->comp_msg[msg_type][index].file_name, file_name, STR_BUF );
	  comp_log->comp_msg[msg_type][index].line = line;
	  comp_log->comp_msg[msg_type][index].id = index;  
	  comp_log->msg_cnt[msg_type] ++;
  	}
    }
  fclose(fp);

  return;
}

int main ( int argc, char* argv[] )
{
  int i, j;

  if(argc <= 1 )
    {
      cout<<"prepair: Please save a compile message as compile_log.txt\n";
      cout<<"usage: ./autoOMP [source file]\n";
      exit(1);
    }
  
  COMP_LOG comp_log;
  read_comp_log( &comp_log );

  SgProject* project = frontend(argc,argv);

  SgFilePtrList & ptr_list = project->get_fileList();
  for (SgFilePtrList::iterator iter = ptr_list.begin(); iter!=ptr_list.end();
       iter++)
    {
      SgFile* sageFile = (*iter);
      SgSourceFile * sfile = isSgSourceFile(sageFile);
      ROSE_ASSERT(sfile);
      SgGlobal *root = sfile->get_globalScope();
      SgDeclarationStatementPtrList& declList = root->get_declarations ();

      //For each function body in the scope
      for (SgDeclarationStatementPtrList::iterator p = declList.begin(); p != declList.end(); ++p) 
	{
	  VariantVector vv = VariantVector(V_SgFortranDo);
	  Rose_STL_Container<SgNode*> loops = NodeQuery::queryMemoryPool(vv);
	  if (0==loops.size()) continue;	  

	  for (Rose_STL_Container<SgNode*>::iterator iter = loops.begin(); iter!= loops.end(); iter++ ) 
	    {
	      SgNode* current_loop = *iter;
	      int line = current_loop->get_file_info()->get_line();

	      for( j = 0 ; j < comp_log.msg_cnt[mul] ; j++ )
		{
		  if (( line == comp_log.comp_msg[mul][j].line ) && ( 1 == comp_log.comp_msg[mul][j].msg_id ))
		    {
		      if( current_loop->variantT()==V_SgFortranDo )
			{
			  string fortran_omp_parallel_do_directive = "!_KZ start\n!$omp parallel\n!$omp do\n!_KZ end\n";
			  SageInterface::addTextForUnparser(current_loop,fortran_omp_parallel_do_directive,AstUnparseAttribute::e_before);
			  string fortran_omp_end_parallel_do_directive = "!_KZ start\n!$omp end do\n!$omp end parallel\n!_KZ end\n";
			  SageInterface::addTextForUnparser(current_loop,fortran_omp_end_parallel_do_directive,AstUnparseAttribute::e_after);	      
			  comp_log.comp_msg[mul][j].msg_id = 0;
			  cout<<"Semi-automatic insertion to a loop at line:"<<current_loop->get_file_info()->get_line()<<endl;
			}
		    }
		}
	    }
	}
    }


  for( i = 0 ; i < 10 ; i ++ )
    {
      free(comp_log.comp_msg[i]);
    }

  return backend(project);
}
