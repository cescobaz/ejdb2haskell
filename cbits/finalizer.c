#include "finalizer.h"
#include <stdlib.h>

void finalizerJQL(JQL* jql) {
  JQL* jqlCopy = jql;
  jql_destroy(jql);
  free(jqlCopy);
}
