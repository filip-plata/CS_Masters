BEGIN {
    FS = "{-";
    skip = 0;
}

/^```agda/ {
    print "```agda";
    next;
}

/^```edn/ {
    skip=1;
    next;
}

/^```$/ {
    if(skip==1)
        skip=0;
    else
        print "```";
    next;
}

{
    if(skip!=1)
        print;
}