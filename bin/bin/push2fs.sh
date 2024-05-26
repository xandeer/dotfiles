#!/usr/bin/env sh

# Usage: ./push2fs.sh -u user -p password  -f file -d remote-dir

while getopts "u:p:f:d:" opt; do
  case "$opt" in
    u) user="$OPTARG";;
    p) pwd="$OPTARG";;
    f) file="$OPTARG";;
    d) dir="$OPTARG";;
    \?) echo "Usage: ./push2fs.sh -u user -p password  -f file -d remote-dir"; exit 1;;
  esac
done

if [ -z "$user" ] || [ -z "$pwd" ] || [ -z "$file" ] || [ -z "$dir" ]; then
  echo "Usage: ./push2fs.sh -u user -p password  -f file -d remote-dir"
  exit 1
fi

echo "Uploading $file to $dir"

# https://docs.nextcloud.com/server/stable/developer_manual/client_apis/WebDAV/basic.html#uploading-files
curl -u $user:$pwd -T $file https://fs.xmind.cn/remote.php/dav/files/$user/$dir/$(basename $file)

# https://docs.nextcloud.com/server/stable/developer_manual/client_apis/OCS/ocs-share-api.html#create-a-new-share
data=$(curl -su $user:$pwd -H "OCS-APIRequest: true" -X POST -d "path=$dir/$(basename $file)&shareType=3" https://fs.xmind.cn/ocs/v2.php/apps/files_sharing/api/v1/shares)

url=$(echo "$data" | sed -n 's/.*<url>\(.*\)<\/url>.*/\1/p')
download_url="$url/download"

echo "Uploaded $file to $download_url"

echo "- [$(basename $file)]($download_url)" | pbcopy
