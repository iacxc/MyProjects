(load "package")

(new-package "Lunmgr::Device" '("Data::Dumper")
             '("uuid" "lun" "wwn" "model" "mpath" "dev" 
               "size" "path" "mount_pt" "name" "msa" "pstatus")
             '("ToPrintStr" "ToPrintXML" "ToTable" "ToJbodStr" "ToJbodXML"))

