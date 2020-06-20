BEGIN { FS=":";}
/^NAME:/ {gsub(/ /, "", $2); name=$2; next;}
/^SURNAME:/ {gsub(/ /, "", $2); surname=$2; next;}
/^MATRICULATION:/ {gsub(/ /, "", $2); matriculation=$2; next;}
{ } END { print "name = " name; print "surname = " surname; print "matriculation = " matriculation;

error=0;

if (name== "") {
    print "ERROR: name missing";
    error=1;
}

if (surname == "") {
    print "ERROR: surname missing";
    error=1;
}

if (matriculation == "") {
    print "ERROR: matriculation missing";
    error=1;
}

exit error;

}