一个简单易用的 RSA公匙加密体系实现。

初次使用：运行并按照提示生成私匙
可以创建一个快捷方式，方便拖动文件。

将你的.contact文件分享给你的朋友。
不要分享.prikey

任何人只需要有公匙（contact里面包含公匙）就可以给这个人发送加密消息
但是只有有私匙(private key)的人才能解密任何被这个公匙加密的消息

导入别人的联系方式：
将.contact文件拖动到prime.exe上。你也可以直接编辑allcontacts.txt。

加密：在命令行下运行
.\Prime -to <谁谁谁> -file <filepath>
.\Prime -to <谁谁谁> "message"
.\Prime -to <谁谁谁>
.\Prime -to <谁谁谁> -string "message"
或：拖动任意文件到exe上面也可以加密文件

解密：在命令行下运行
.\Prime -d
.\Prime -d "message"
.\Prime -d -file filepath
或：拖动 .encrypt 文件到exe上面也可以解密文件


注：

由于windows 命令行的阴间特性
如果你用的是拖动，被拖动的文件需要和prikey在同一个文件夹才能正常工作
不过，创建快捷方式并拖动到快捷方式上可以解决此问题（
不然你拖一个别的文件夹的文件上去，它会提示你创建prikey, 但实际上你已经在prime.exe同文件夹下创建过了
 
 
