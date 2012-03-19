BEGIN {
  in_module = "";
  in_module_line = 0;
  line = 0;
  total_line = 0;
}

/^module/ {
  in_module = $2;
  in_module_line = 1;
  total_line = total_line + 1;
}

/^end/ {
  print in_module " " in_module_line;
  in_module = "";
}

/.*/ {
  if (in_module != "")
    in_module_line = in_module_line + 1;
  else
    line = line + 1;
  total_line = total_line + 1;
}

END {
  print "not in module " line;
  print "total lines " total_line;
}
