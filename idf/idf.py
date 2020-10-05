#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Interface Definition Framework

@author: Jonathan Schilling (jonathan.schilling@mail.de)
"""

import os # linesep



# data container class for all information specifying a variable
class Variable:
    name = None
    isParameter = False
    description = None
    dtype = None
    defaultValue = None
    rank = 0 # scalar by default
    unit = None
    startingIndices = None
    maximumIndices = None
    
    def __init__(self, name):
        self.name = name
    
    def setIsParameter(self, isParameter):
        self.isParameter = isParameter
    
    def setDescription(self, description):
        self.description = description
    
    def setType(self, dtype):
        self.dtype = dtype
    
    def setRank(self, rank):
        self.rank = rank
        
    def setDefaultValue(self, defaultValue):
        self.defaultValue = defaultValue
        
    def setUnit(self, unit):
        self.unit = unit
    
    def setStartingIndices(self, startingIndices):
        self.startingIndices = startingIndices
    
    def setMaximumIndices(self, maximumIndices):
        self.maximumIndices = maximumIndices
    
    def __str__(self):
        if self.isParameter:
            description = "[parameter] "
        else:
            description = "[ variable] "
        description += self.name+os.linesep
        dtypeDesc = "         dtype: "
        dtypeDescLen = len(dtypeDesc)
        description += dtypeDesc
        if type(self.dtype) is list:
            numTypes = len(self.dtype)
            for i,t in enumerate(self.dtype):
                indentedDesc= indented(dtypeDescLen, str(t), " ")
                if i==0:
                    description += indentedDesc[dtypeDescLen:]
                else:
                    description += indentedDesc
                if i < numTypes-1:
                    description += ","+os.linesep
        else:
            description += str(self.dtype)
        description += os.linesep
        
        if self.defaultValue is not None:
            description += " default value: "+str(self.defaultValue)+os.linesep
            
        if self.unit is not None:
            description += "          unit: "+self.unit+os.linesep
        
        if self.rank > 0:
            description += "          rank: "+str(self.rank)+os.linesep
            
            if self.startingIndices is not None and len(self.startingIndices) != self.rank:
                raise RuntimeError("length of startingIndices is not equal to specified rank for "+self.name)
            
            # maximumIndices are needed always
            if len(self.maximumIndices) != self.rank:
                raise RuntimeError("length of  maximumIndices is not equal to specified rank for "+self.name)
             
            rankDesc = "["
            for r in range(self.rank):
                startingIndex = "1"
                if self.startingIndices is not None and self.startingIndices[r] is not None and self.startingIndices[r].strip() != "":
                    startingIndex = self.startingIndices[r]
                maximumIndex = self.maximumIndices[r]
                rankDesc += startingIndex+":"+maximumIndex
                if r < self.rank-1:
                    rankDesc += ", "
            rankDesc += "]"
            
            description += "    dimensions: "+rankDesc+os.linesep
        
        if self.description is not None:
            description += toDoc(self.description, doc_fmt="raw")
        
        return description



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





# indent a string (which might consist of multiple lines) by a given number of tabs or
# some other given character
def indented(tabs, strInput, indentationChar="\t"):
    indentation = ''
    for i in range(tabs):
        indentation += indentationChar
    indented = ''
    if os.linesep in strInput:
        lines = strInput.split(os.linesep)
        for line in lines[:-1]:
            indented += indentation+line+os.linesep
        indented += indentation+lines[-1]
        if strInput[-1] == os.linesep:
            indented += os.linesep
    else:
        indented = indentation+strInput
    return indented

# convert the description item from a Variable into the corresponding documentation
def toDoc(desc, doc_fmt="html"):
    if   type(desc) is str:
        return desc
    elif type(desc) is dict:
        return desc_dictToDoc(desc, doc_fmt)
    elif type(desc) is list:
        return desc_listToDoc(desc, doc_fmt)
    
    elif desc is not None:
        raise TypeError("what is this that you want to document of type "+str(type(desc))+"?")
    else:
        return ""

# convert a dict from a Variable's description into the corresponding documentation
def desc_dictToDoc(desc_dict, doc_fmt="html"):
    if type(desc_dict) is not dict:
        raise RuntimeError("desc_dictToDoc was called with "+str(type(desc_dict))+" instead of dict")
    result = ""
    for iKey,key in enumerate(desc_dict):
        if type(key) is not str:
            raise RuntimeError("desc_dictToDoc was given a dict with key type "+str(type(desc_dict))+" instead of str")
        if iKey>0:
            result += os.linesep
        result += key
        if desc_dict[key] is not None:
            result += os.linesep+toDoc(desc_dict[key], doc_fmt)
    return result

# convert a list from a Variable's description into the corresponding documentation
def desc_listToDoc(desc_list, doc_fmt="html"):
    startList = "<ul>"+os.linesep
    endList = os.linesep+"</ul>"
    startItem = "<li> "
    endItem = " </li>"
    if doc_fmt == "raw":
        startList = ""
        endList = ""
        startItem = "* "
        endItem = ""
    elif doc_fmt != "html":
        raise RuntimeError("format '"+doc_fmt+"' not supported!")
    
    lenStartItem = len(startItem)
    numItems = len(desc_list)
    
    listDesc = startList
    for i,item in enumerate(desc_list):
        itemStr = toDoc(item, doc_fmt)
        # indent the item content by length of startItem so that it is nicely aligned
        # first item shall not be indented => [lenStartItem:]
        liIndented = startItem+indented(lenStartItem, itemStr, " ")[lenStartItem:]+endItem
        listDesc += liIndented
        if i < numItems-1:
            listDesc += os.linesep
    listDesc += endList
    return listDesc

