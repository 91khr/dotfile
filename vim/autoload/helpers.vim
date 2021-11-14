vim9script
# ======================================================================================================================
# Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
# Author: Virginia Senioria
# Senioriae VIm configuration: Helpers etc.

def helpers#GetOutputFile(srcfile: string, outsuffix: string, outdir: string = ''): string
    if !!outdir | outdir = srcfile->fnamemodify(':h') | endif
enddef

