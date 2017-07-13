
require './LibconvBase.rb'
#load './LibconvBase.rb'

dwt=generate_dwt($optims)
iwt=generate_iwt($optims)
mf=generate_mf($optims)
imf=generate_imf($optims)
s1r=generate_s1tor($optims)
rs1=generate_rtos1($optims)

files=[dwt.dump_to_file(),iwt.dump_to_file(),mf.dump_to_file(),imf.dump_to_file(),rs1.dump_to_file(),s1r.dump_to_file()]

raise if files.length() != files.to_set().length()

makelines="""
lib_LIBRARIES = libconv.a

#temporary compiling line for gfortran
AM_FCFLAGS = -I. -O

libconv_a_SOURCES = """

files.each{|f| makelines+=' '+f.to_str}

File::open("src/Makefile.am","w") {|f|
  f.puts makelines
    }







