set -xe

${coreutils}/bin/mkdir -p ${out}
${coreutils}/bin/cp ${frontend}/bin/frontend.jsexe/* ${out}/
${coreutils}/bin/cp ${backend}/bin/backend ${out}/
