#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 15:17:47 2019

@author: Jonathan Schilling (jonathan.schilling@ipp.mpg.de)
"""

# prepare for code generation

from .idf import Variable, toDoc, indented

# data container class for all information specifying a namelist
class Namelist(object):
    
    name = None
    description = None
    variables = None
    
    def __init__(self, name):
        self.name = name
        self.variables = []
    
    def setDescription(self, description):
        self.description = description
    
    def addVariable(self, var):
        if type(var) is not Variable:
            raise TypeError("type of var is not Variable but '"+str(type(var))+"'")
        self.variables.append(var)

    def addVariables(self, listOfVars):
        if type(listOfVars) is not list:
            raise TypeError("type of listOfVars is not list but '"+str(type(listOfVars))+"'")
        for var in listOfVars:
            self.addVariable(var)

# datatype in Fortran from specification file
def dtype(dtype):
    if dtype=='int':
        return 'integer'
    elif dtype=='double':
        return 'real'
    elif dtype=='boolean':
        return 'logical'
    else:
        return 'type('+str(dtype)+')'

def val(val):
    if type(val) is bool:
        if val:
            return ".TRUE."
        else:
            return ".FALSE."
    else:
        return str(val)

def commentOut(multilineString, commentDirection = ">"):
    comment = "!"+commentDirection+" "
    if type(multilineString) is not str:
        raise TypeError("type of given multilineString should be str, not "+
                        str(type(multilineString))+
                        "; how to comment out this?")
    lines = multilineString.split("\n")
    result = ""
    for line in lines[:-1]:
        result += comment+line+"\n"
    result += comment+lines[-1]
    return result

# declare a variable including dimensions(TODO) and doxygen-compatible comments
def declareVariable(var, attachDescription=True, refDeclLength=None):
    if type(var) is Variable:
        decl = dtype(var.dtype)
        
        if var.isParameter:
            decl += ", parameter"
        
        decl += " :: "
        decl += var.name
        
        if var.rank>0:
            if var.maximumIndices is None:
                raise ValueError("maximumIndices of Variable '"+var.name+
                                 "' not set")
            
            # add array dimensions if rank>0
            decl += "("
            
            # default start index is 1 in Fortran
            startIdx = ["1"]*var.rank
            if var.startingIndices is not None:
                if len(var.startingIndices) != var.rank:
                    raise ValueError("number of starting indices should match rank of Variable '"+var.name+"'")
                for i in range(var.rank):
                    if var.startingIndices[i] is not None:
                        startIdx[i] = var.startingIndices[i]
            if var.maximumIndices is None:
                raise ValueError("maximumIndices not set for rank-"+str(var.rank)+" Variable '"+var.name+"'")
            else:
                if len(var.maximumIndices) != var.rank:
                    raise ValueError("number of maximum indices should match rank of Variable '"+var.name+"'")
            
            decl += startIdx[0]+":"+var.maximumIndices[0]
            for i in range(1,var.rank):
                decl += ", "+startIdx[i]+":"+var.maximumIndices[i]
            decl += ")"
        
        decl += " = "
        decl += val(var.defaultValue)
        if refDeclLength is None:
            # take length of this declaration as reference if no external one proviede
            refDeclLength = len(decl)
        if attachDescription:
            decl += " "
            decl_doc = commentOut(toDoc(var.description), commentDirection="<")
            if "\n" in decl_doc:
                # indent all but first line by length of declaration,
                # so that the following documentation lines are aligned
                # first line has to be indented the difference between decl length
                # and desired indentLength
                docparts = decl_doc.split("\n")
                if len(decl) <= refDeclLength:
                    decl += " "*(refDeclLength-len(decl)+1)
                decl += docparts[0]+"\n"
                decl += indented(refDeclLength+1, "\n".join(docparts[1:]), " ")
            else:
                if len(decl) <= refDeclLength:
                    decl += " "*(refDeclLength-len(decl)+1)
                decl += decl_doc
        return decl.strip()
    else:
        raise TypeError("var "+var.name+" is not a adf.Variable")

def declareNamelist(nml):
    result  = commentOut(toDoc(nml.description))+"\n"
    result += "namelist /"+nml.name+"/ &\n"
    for var in nml.variables[:-1]:
        result += " "+var.name+" ,&\n"
    result += " "+nml.variables[-1].name
    return result

def readHdf5Group(name, contents):
    if type(contents) is list:
        print("read group '"+name+"' from HDF5 file")
        for item in contents:
            if type(item) is Variable:
                readHdf5Dataset(item)
            elif type(item) is Namelist:
                readHdf5Group(item.name, item.variables)
    elif type(contents) is Namelist:
        print("read namelist '"+contents.name+"' from HDF5 file")
        readHdf5Group(contents.name, contents.variables)

def readHdf5Dataset(item):
    print("read dataset "+item.name)
    

















# generate custom compound datatype declaration in Fortran
def genType(name, members):
    ret = 'TYPE '+name+'\n'
    for member in members:
        if type(member) == Group or type(member) == Datatype:
            ret += '    TYPE('+member.name+')'
        else:
            ret += '    '+dtype(member.dtype)
            if member.rank>0:
                ret += ', ALLOCATABLE'
        ret += ' :: '+member.name
        if type(member) != Group and member.rank>0:
            ret += '('
            for i in range(member.rank):
                if i>0:
                    ret += ',:'
                else:
                    ret += ':'
            ret += ')'
        ret += '\n'
    ret += 'END TYPE '+name
    return ret

# initial code of loading routine
def startLoader(f):
    f.write("""subroutine loadSpec(s, filename, ierr)
  use hdf5
  implicit none
  type(SpecOutput), intent(inout) :: s                 ! target datastructure
  character(len=*), intent(in)    :: filename          ! filename to load
  integer, intent(out), optional  :: ierr              ! error flag; .eq.0 if ok
  integer                         :: hdfier            ! error flag for HDF5 API calls
  integer(hid_t)                  :: file_id           ! identifier for current file
  integer(hid_t)                  :: dset_id           ! temporary dataset id
  integer(hid_t)                  :: dataspace         ! dataspace used to query dataset size
  integer(hsize_t)                :: dims_1(1)         ! current dimensions of rank-1 dataset
  integer(hsize_t)                :: dims_2(2)         ! current dimensions of rank-2 dataset
  integer(hsize_t)                :: dims_3(3)         ! current dimensions of rank-3 dataset
  integer(hsize_t)                :: max_dims_1(1)     ! maximum dimensions of rank-1 dataset
  integer(hsize_t)                :: max_dims_2(2)     ! maximum dimensions of rank-2 dataset
  integer(hsize_t)                :: max_dims_3(3)     ! maximum dimensions of rank-3 dataset
  integer                         :: logical_tmp       ! temporary integer used to read logicals
  
  call h5open_f(hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error opening HDF5 library" ; goto 9999 ; endif

  call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error opening HDF5 file '",filename,"'" ; goto 9998 ; endif
""")

# finalizing code of loading routine
def endLoader(f):
    f.write("""
9998 continue
  
  call h5fclose_f(file_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing HDF5 file '",filename,"'" ; ierr = hdfier ; endif

9999 continue

  call h5close_f(hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing HDF5 library" ; ierr = hdfier ; endif 
    
end subroutine loadSpec
""")

# write demo code
def demoLoader(f):
    f.write("""
program test_read_spec
  use read_spec
  implicit none
  type(SpecOutput) :: s
  character(*), parameter :: filename = "/home/jonathan/Uni/04_PhD/00_programs/SPEC/SPEC/InputFiles/TestCases/G3V02L1Fi.001.h5"
  
  write(*,*) "reading '",filename,"'..."
  call loadSpec(s, filename)
  write(*,*) "done"
  
  write(*,"(A,F4.2)") "SPEC version: ", s%version
  write(*,"(A,99I2)") "Lrad:", s%input%physics%Lrad
  
  call freeSpec(s)
end program test_read_spec
""")

# read a scalar (int or double) from HDF5 variable srcPath into the source code variable targetPath
def loadItem(f, item):
    
    srcName    = item.getFullName()
    
    targetName = "s"+srcName.replace("/","%")
    if item.rank>0:
        targetName += "("
        if item.indexMapping is not None:
            for dim,idxRange in enumerate(item.indexMapping):
                if dim==0:
                    targetName += idxRange
                else:
                    targetName += ", "+idxRange
        else:
            for dim in range(item.rank):
                if dim==0:
                    targetName += "1:dims_"+str(item.rank)+"(1)"
                else:
                    targetName += ", 1:dims_"+str(item.rank)+"("+str(dim+1)+")"
        targetName += ")"
    
    #print("read {} into {}".format(srcName, targetName))
    
    # translate dtype into HDF5 type
    h5type='ERROR'
    if item.dtype=='double':
        h5type='H5T_NATIVE_DOUBLE'
    elif item.dtype=='int' or item.dtype=='boolean':
        h5type='H5T_NATIVE_INTEGER'
    else:
        h5type='TYPE('+item.dtype.upper()+')'
    
        
    
    
    if item.rank==0:
        if (item.dtype=='boolean'):
            fmt="""
! {srcName} --> {targetName}; rank={rank}; h5type={h5type}
  call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
  call h5dread_f(dset_id, {h5type}, logical_tmp, int((/1/), HSIZE_T), hdfier)
  {targetName} = merge(.TRUE., .FALSE., logical_tmp.ne.0)
  if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
  call h5dclose_f(dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
"""
        else:
            fmt="""
! {srcName} --> {targetName}; rank={rank}; h5type={h5type}
  call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
  call h5dread_f(dset_id, {h5type}, {targetName}, int((/1/), HSIZE_T), hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
  call h5dclose_f(dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
"""
    else:
        if (item.dtype=='boolean'):
            print("ERROR: cannot generate reader for logical array '"+srcName+"' yet!")
        fmt="""
! {srcName} --> {targetName}; rank={rank}
  call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
  
  ! open dataspace to get current state of dataset
  call h5dget_space_f(dset_id, dataspace, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error getting dataspace for dataset '{srcName}'" ; goto 9998 ; endif
  
  ! get current size of dataset
  call h5sget_simple_extent_dims_f(dataspace, dims_{rank}, max_dims_{rank}, hdfier)
  if (hdfier.ne.{rank}) then ; write(*,*) "unexpected rank of dataset '{srcName}': ",hdfier," .ne. {rank}" ; goto 9998 ; endif

  ! close dataspace after it has been used to query the size of the variable
  call h5sclose_f(dataspace, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing dataspace for dataset '{srcName}'" ; goto 9998 ; endif
  
  allocate({targetName})
  
  call h5dread_f(dset_id, {h5type}, {targetName}, dims_{rank}, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
  
  call h5dclose_f(dset_id, hdfier)
  if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
"""
    f.write(fmt.format(srcName=srcName, targetName=targetName, h5type=h5type, rank=item.rank))
    
# initial code of loading routine
def startFree(f):
    f.write("""subroutine freeSpec(s)
  implicit none
  type(SpecOutput), intent(inout) :: s ! datastructure to free
""")

# finalizing code of loading routine
def endFree(f):
    f.write("""end subroutine freeSpec
""")

# free an allocated item of rank .ge. 1
def freeItem(f, item):
    
    srcName    = item.getFullName()
    targetName = "s"+srcName.replace("/","%")
    
    if (item.rank > 0):
        print("free {}".format(targetName))
        f.write("  deallocate("+targetName+")\n")
