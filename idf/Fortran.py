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
def dtype(dtype, maximumIndices = None):
    if dtype=='int':
        return 'integer'
    elif dtype=='double':
        return 'real'
    elif dtype=='boolean':
        return 'logical'
    elif dtype=='string':
        if maximumIndices is None:
            return 'character(len=*)'
        elif type(maximumIndices) is str:
            return 'character(len='+maximumIndices+')'
    else:
        return 'type('+str(dtype)+')'

def val(val):
    if type(val) is bool:
        if val:
            return ".TRUE."
        else:
            return ".FALSE."
    elif type(val) is str:
        return "'"+val+"'"
    elif type(val) is Variable:
        return val.name
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
        decl = dtype(var.dtype, var.maximumIndices)
        
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


# The following scheme to verify the existence, correct type and reasonable value of the Dataset
# is outlined in the documentation of the e.g. h5oexists_by_name_f library routine:
# https://portal.hdfgroup.org/display/HDF5/H5O_EXISTS_BY_NAME
# The procedure consists essentially of the following steps logically nested within each other:
#  1. check if the link with the given Dataset or Group name exists
#  2. check that the link at the given name resolves to an object
#  3. open the object at the given name
#  4. query the object type of the just-opened object
#  5. get the datatype of the object
#  6. convert the datatype into the native datatype for endianess-agnostic comparison
#  7. compare the native datatype with the expected native datatype
#  8. read the Dataset using the verified native datatype
#  9. close the native datatype
# 10. close the Dataset object

def _readGroupContents(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    output = ""
    if type(contents) is list:
        print("read group '"+name+"' from HDF5 file")
        for item in contents:
            if type(item) is Variable:
                output += readHdf5Dataset("grp_"+name, item, numTabs, indentationChar)+"\n"
            elif type(item) is Namelist:
                output += readHdf5Group("grp_"+name, item.name, item.variables, numTabs, indentationChar)+"\n"
    elif type(contents) is Namelist:
        print("read namelist '"+contents.name+"' from HDF5 file")
        output += readHdf5Group("grp_"+name, contents.name, contents.variables, numTabs, indentationChar)+"\n"
    
    # [:-1] to remove last '\n'
    return output[:-1]

def _checkIsGroup(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _readGroupContents,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _readGroupContents
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
        
    output = ("! check if "+name+" is a Group"+"\n"
              +"if (item_type.ne.H5I_GROUP_F) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error verifying that object "+name+" is a Group(\",H5I_GROUP_F,\"); rather it is \",item_type", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully verified that "+name+" is a Group\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! check if "+name+" is a Group")
    return output

def _queryType(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _checkIsGroup,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _checkIsGroup
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! query type of "+name+" object"+"\n"
              +"call h5iget_type_f("+enclosingGroup+", item_type, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error querying type of object "+name+"\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully queried item type of "+name+"\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! query type of "+name+" object")
    return output

def _closeObject(enclosingGroup, name, contents, numTabs=1, indentationChar="\t"):
    output = ("call h5oclose_f(grp_"+name+", hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error closing object "+name+"\"", indentationChar)+"\n"
              +"elseif (verbose) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"successfully closed object "+name+"\"", indentationChar)+"\n"
              +"endif")
    return output

def _openObject(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _queryType,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _queryType
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! try to open "+name+" object"+"\n"
              +"call h5oopen_f("+enclosingGroup+", \""+name+"\", grp_"+name+", hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error opening object "+name+"\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully opened object at "+name+"\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations)+"\n"
                        +_closeObject(enclosingGroup, name, contents, numTabs, indentationChar), indentationChar)+"\n"
              +"endif ! try to open "+name+" object")
    return output

def _objectAtLink(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _openObject,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _openObject
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! query existence of object at "+name+" link"+"\n"
              +"call h5oexists_by_name_f("+enclosingGroup+", \""+name+"\", item_exists, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error checking for presence of object at "+name+"\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully checked for presence of an object at "+name+" link\" ; endif"+"\n"
                      +"! check that there exists an item at the "+name+" link"+"\n"
                      +"if (item_exists) then"+"\n"
                      +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully checked that there exists an item at "+name+"\" ; endif"+"\n"
                                +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar), indentationChar)+"\n"
              +indented(numTabs, "else"+"\n"
                       +indented(numTabs, "! "+name+" link present but does not resolve to any object"+"\n"
                                 +"write(ounit,*) \""+name+" link present but does not resolve to any object\"", indentationChar)+"\n"
                       +"endif ! check that there exists an item at the "+name+" link", indentationChar)+"\n"
              +"endif ! query existence of object at "+name+" link")
    return output

def _linkExists(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _objectAtLink,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _objectAtLink
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! query existence of "+name+" link"+"\n"
              +"call h5lexists_f("+enclosingGroup+", \""+name+"\", item_exists, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error checking if "+name+" link exists\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully checked if the "+name+" link exists\" ; endif"+"\n"
                        +"! check that "+name+" link exists"+"\n"
                        +"if (item_exists) then"+"\n"
                        +indented(numTabs, "if (verbose) then; write(ounit,*) \"successfully checked that the link "+name+" exists\" ; endif"+"\n"
                                  +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar), indentationChar)+"\n"
              +indented(numTabs, "else"+"\n"
                       +indented(numTabs, "! "+name+" link not present in input file"+"\n"
                                 +"write(ounit,*) \"WARNING: "+name+" link not found\"", indentationChar)+"\n"
                       +"endif ! check if "+name+" link exists", indentationChar)+"\n"
              +"endif ! query existence of "+name+" link")
    return output

def _readScalarInt(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    return "! TODO: actually read "+name

def _datatypesEqual(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _readScalarInt,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _readScalarInt
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! verify correct datatype of /input/physics/Igeometry"+"\n"
              +"if (datatypes_equal) then"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully checked that datatype of /input/physics/Igeometry is H5T_NATIVE_INTEGER :-)\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "write(ounit,*) \"ERROR: native datatype of /input/physics/Igeometry should be H5T_NATIVE_INTEGER but is \",dtype_id_native", indentationChar)+"\n"
              +"endif !verify correct datatype of /input/physics/Igeometry")
    return output

def _nativeDatatypeIsInteger(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _datatypesEqual,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _datatypesEqual
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! call comparison routine for /input/physics/Igeometry datatype"+"\n"
              +"call h5tequal_f(dtype_id_native, H5T_NATIVE_INTEGER, datatypes_equal, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error comparing datatype of /input/physics/Igeometry to H5T_NATIVE_INTEGER\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully executed comparison of datatype of /input/physics/Igeometry with H5T_NATIVE_INTEGER\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! call comparison routine for /input/physics/Igeometry datatype")
    return output

def _closeDatatype(enclosingGroup, name, contents, numTabs=1, indentationChar="\t"):
    output = ("! close datatype of /input/physics/Igeometry"+"\n"
              +"call h5tclose_f(dtype_id, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error closing datatype of /input/physics/Igeometry\"", indentationChar)+"\n"
              +"elseif (verbose) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"successfully closed datatype of /input/physics/Igeometry\"", indentationChar)+"\n"
              +"endif ! close datatype of /input/physics/Igeometry")
    return output

def _convertDatatypeToNative(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _nativeDatatypeIsInteger,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _nativeDatatypeIsInteger
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
        
    output = ("! convert datatype to native array for comparison"+"\n"
              +"call h5tget_native_type_f(dtype_id, 1, dtype_id_native, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error converting datatype of /input/physics/Igeometry to native datatype\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully converted datatype of /input/physics/Igeometry to native datatype\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations)+"\n"
                        +_closeDatatype(enclosingGroup, name, contents, numTabs, indentationChar), indentationChar)+"\n"
              +"endif ! convert datatype to native array for comparison")
    return output

def _queryDatatype(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _convertDatatypeToNative,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _convertDatatypeToNative
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
    
    output = ("! query datatype of /input/physics/Igeometry"+"\n"
              +"call h5dget_type_f(dset_input_physics_Igeometry, dtype_id, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error querying datatype of /input/physics/Igeometry\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully queried datatype of /input/physics/Igeometry\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! query datatype of /input/physics/Igeometry")
    return output
    

def _isScalarDataspace(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _queryDatatype,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _queryDatatype
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]

    output = ("! check that the type of the /input/physics/Igeometry dataspace is H5S_SCALAR_F"+"\n"
              +"if (dspace_type.eq.H5S_SCALAR_F) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"successfully verified that the type of the /input/physics/Igeometry dataspace is H5S_SCALAR_F\""+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "write(ounit,*) \"ERROR: type of dataspace /input/physics/Igeometry is not H5S_SCALAR_F but \",dspace_type", indentationChar)+"\n"
              +"endif ! check that the type of the /input/physics/Igeometry dataspace is H5S_SCALAR_F")
    return output

def _getDataspaceType(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _isScalarDataspace,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _isScalarDataspace
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]

    output = ("! determine the type of the dataspace"+"\n"
              +"call h5sget_simple_extent_type_f(dataspace, dspace_type, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error getting type of /input/physics/Igeometry dataspace\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "! check that the type of the /input/physics/Igeometry dataspace is H5S_SCALAR_F"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! open dataspace of /input/physics/Igeometry")
    return output
    
    

def _closeDataspace(enclosingGroup, name, contents, numTabs=1, indentationChar="\t"):
    output=("! close dataspace of /input/physics/Igeometry"+"\n"
            +"call h5sclose_f(dataspace, hdfier)"+"\n"
            +"if (hdfier.ne.0) then ; write(ounit,*) \"error closing dataspace of /input/physics/Igeometry\" ; endif")
    return output

def _openDataspace(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _isScalarDataspace,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _getDataspaceType
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
        
    output = ("! open dataspace of /input/physics/Igeometry"+"\n"
              +"call h5dget_space_f(dset_input_physics_Igeometry, dataspace, hdfier)"+"\n"
              +"if (hdfier.ne.0) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error getting dataspace of /input/physics/Igeometry\"", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully got dataspace of "+name+"\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations)+"\n"
                        +_closeDataspace(enclosingGroup, name, contents, numTabs, indentationChar), indentationChar)+"\n"
              +"endif ! open dataspace of /input/physics/Igeometry")
    return output


def _checkIsDataset(enclosingGroup, name, contents, numTabs=1, indentationChar="\t", nestedOperations=None):
    # by default, the next nested step is to call _openDataspace,
    # but allow for flexibitly by checking provided nestedOperations
    nestedOperation = _openDataspace
    nextOperations = None
    if nestedOperations is not None and type(nestedOperations) is list and len(nestedOperations) > 0:
        nestedOperation = nestedOperations[0]
        nextOperations = nestedOperations[1:]
        
    output = ("! check if "+name+" is a Dataset"+"\n"
              +"if (item_type.ne.H5I_DATASET_F) then"+"\n"
              +indented(numTabs, "write(ounit,*) \"error verifying that object "+name+" is a Dataset(\",H5I_DATASET_F,\"); rather it is \",item_type", indentationChar)+"\n"
              +"else"+"\n"
              +indented(numTabs, "if (verbose) then ; write(ounit,*) \"successfully verified that "+name+" is a Dataset\" ; endif"+"\n"
                        +nestedOperation(enclosingGroup, name, contents, numTabs, indentationChar, nextOperations), indentationChar)+"\n"
              +"endif ! check if "+name+" is a Group")
    return output




def readHdf5Group(enclosingGroup, name, contents, numTabs=1, indentationChar="\t"):
    # nested operations to be performed to (reliably and error-proof)
    # read the contents of a HDF5 group
    return _linkExists(enclosingGroup, name, contents, numTabs, indentationChar,
                        [_objectAtLink,
                        _openObject,
                        _queryType,
                        _checkIsGroup,
                        _readGroupContents])




def readHdf5Dataset(enclosingGroup, item, numTabs=1, indentationChar="\t"):
    # TODO: build list of nested operations based on given Variable:
    # rank need to be checked
    # also dtype needs to match
    
    return _linkExists(enclosingGroup, item.name, None, numTabs, indentationChar,
                        [_objectAtLink,
                        _openObject,
                        _queryType,
                        _checkIsDataset])

















# # generate custom compound datatype declaration in Fortran
# def genType(name, members):
#     ret = 'TYPE '+name+'\n'
#     for member in members:
#         if type(member) == Group or type(member) == Datatype:
#             ret += '    TYPE('+member.name+')'
#         else:
#             ret += '    '+dtype(member.dtype)
#             if member.rank>0:
#                 ret += ', ALLOCATABLE'
#         ret += ' :: '+member.name
#         if type(member) != Group and member.rank>0:
#             ret += '('
#             for i in range(member.rank):
#                 if i>0:
#                     ret += ',:'
#                 else:
#                     ret += ':'
#             ret += ')'
#         ret += '\n'
#     ret += 'END TYPE '+name
#     return ret

# # initial code of loading routine
# def startLoader(f):
#     f.write("""subroutine loadSpec(s, filename, ierr)
#   use hdf5
#   implicit none
#   type(SpecOutput), intent(inout) :: s                 ! target datastructure
#   character(len=*), intent(in)    :: filename          ! filename to load
#   integer, intent(out), optional  :: ierr              ! error flag; .eq.0 if ok
#   integer                         :: hdfier            ! error flag for HDF5 API calls
#   integer(hid_t)                  :: file_id           ! identifier for current file
#   integer(hid_t)                  :: dset_id           ! temporary dataset id
#   integer(hid_t)                  :: dataspace         ! dataspace used to query dataset size
#   integer(hsize_t)                :: dims_1(1)         ! current dimensions of rank-1 dataset
#   integer(hsize_t)                :: dims_2(2)         ! current dimensions of rank-2 dataset
#   integer(hsize_t)                :: dims_3(3)         ! current dimensions of rank-3 dataset
#   integer(hsize_t)                :: max_dims_1(1)     ! maximum dimensions of rank-1 dataset
#   integer(hsize_t)                :: max_dims_2(2)     ! maximum dimensions of rank-2 dataset
#   integer(hsize_t)                :: max_dims_3(3)     ! maximum dimensions of rank-3 dataset
#   integer                         :: logical_tmp       ! temporary integer used to read logicals
  
#   call h5open_f(hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error opening HDF5 library" ; goto 9999 ; endif

#   call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error opening HDF5 file '",filename,"'" ; goto 9998 ; endif
# """)

# # finalizing code of loading routine
# def endLoader(f):
#     f.write("""
# 9998 continue
  
#   call h5fclose_f(file_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing HDF5 file '",filename,"'" ; ierr = hdfier ; endif

# 9999 continue

#   call h5close_f(hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing HDF5 library" ; ierr = hdfier ; endif 
    
# end subroutine loadSpec
# """)

# # write demo code
# def demoLoader(f):
#     f.write("""
# program test_read_spec
#   use read_spec
#   implicit none
#   type(SpecOutput) :: s
#   character(*), parameter :: filename = "/home/jonathan/Uni/04_PhD/00_programs/SPEC/SPEC/InputFiles/TestCases/G3V02L1Fi.001.h5"
  
#   write(*,*) "reading '",filename,"'..."
#   call loadSpec(s, filename)
#   write(*,*) "done"
  
#   write(*,"(A,F4.2)") "SPEC version: ", s%version
#   write(*,"(A,99I2)") "Lrad:", s%input%physics%Lrad
  
#   call freeSpec(s)
# end program test_read_spec
# """)

# # read a scalar (int or double) from HDF5 variable srcPath into the source code variable targetPath
# def loadItem(f, item):
    
#     srcName    = item.getFullName()
    
#     targetName = "s"+srcName.replace("/","%")
#     if item.rank>0:
#         targetName += "("
#         if item.indexMapping is not None:
#             for dim,idxRange in enumerate(item.indexMapping):
#                 if dim==0:
#                     targetName += idxRange
#                 else:
#                     targetName += ", "+idxRange
#         else:
#             for dim in range(item.rank):
#                 if dim==0:
#                     targetName += "1:dims_"+str(item.rank)+"(1)"
#                 else:
#                     targetName += ", 1:dims_"+str(item.rank)+"("+str(dim+1)+")"
#         targetName += ")"
    
#     #print("read {} into {}".format(srcName, targetName))
    
#     # translate dtype into HDF5 type
#     h5type='ERROR'
#     if item.dtype=='double':
#         h5type='H5T_NATIVE_DOUBLE'
#     elif item.dtype=='int' or item.dtype=='boolean':
#         h5type='H5T_NATIVE_INTEGER'
#     else:
#         h5type='TYPE('+item.dtype.upper()+')'
    
        
    
    
#     if item.rank==0:
#         if (item.dtype=='boolean'):
#             fmt="""
# ! {srcName} --> {targetName}; rank={rank}; h5type={h5type}
#   call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
#   call h5dread_f(dset_id, {h5type}, logical_tmp, int((/1/), HSIZE_T), hdfier)
#   {targetName} = merge(.TRUE., .FALSE., logical_tmp.ne.0)
#   if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
#   call h5dclose_f(dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
# """
#         else:
#             fmt="""
# ! {srcName} --> {targetName}; rank={rank}; h5type={h5type}
#   call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
#   call h5dread_f(dset_id, {h5type}, {targetName}, int((/1/), HSIZE_T), hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
#   call h5dclose_f(dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
# """
#     else:
#         if (item.dtype=='boolean'):
#             print("ERROR: cannot generate reader for logical array '"+srcName+"' yet!")
#         fmt="""
# ! {srcName} --> {targetName}; rank={rank}
#   call h5dopen_f(file_id, "{srcName}", dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error opening dataset '{srcName}'" ; goto 9998 ; endif
  
#   ! open dataspace to get current state of dataset
#   call h5dget_space_f(dset_id, dataspace, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error getting dataspace for dataset '{srcName}'" ; goto 9998 ; endif
  
#   ! get current size of dataset
#   call h5sget_simple_extent_dims_f(dataspace, dims_{rank}, max_dims_{rank}, hdfier)
#   if (hdfier.ne.{rank}) then ; write(*,*) "unexpected rank of dataset '{srcName}': ",hdfier," .ne. {rank}" ; goto 9998 ; endif

#   ! close dataspace after it has been used to query the size of the variable
#   call h5sclose_f(dataspace, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing dataspace for dataset '{srcName}'" ; goto 9998 ; endif
  
#   allocate({targetName})
  
#   call h5dread_f(dset_id, {h5type}, {targetName}, dims_{rank}, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error reading dataset '{srcName}'" ; goto 9998 ; endif
  
#   call h5dclose_f(dset_id, hdfier)
#   if (hdfier.ne.0) then ; write(*,*) "error closing dataset '{srcName}'" ; goto 9998 ; endif
# """
#     f.write(fmt.format(srcName=srcName, targetName=targetName, h5type=h5type, rank=item.rank))
    
# # initial code of loading routine
# def startFree(f):
#     f.write("""subroutine freeSpec(s)
#   implicit none
#   type(SpecOutput), intent(inout) :: s ! datastructure to free
# """)

# # finalizing code of loading routine
# def endFree(f):
#     f.write("""end subroutine freeSpec
# """)

# # free an allocated item of rank .ge. 1
# def freeItem(f, item):
    
#     srcName    = item.getFullName()
#     targetName = "s"+srcName.replace("/","%")
    
#     if (item.rank > 0):
#         print("free {}".format(targetName))
#         f.write("  deallocate("+targetName+")\n")
