# LazSimpleHTTPsGet

![Example Application](https://github.com/dryas/LazSimpleHTTPsGet/blob/main/doc/example_screenshot.png)

LazSimpleHTTPsGet is a simple asynchronous (threads) file download Unit with progress indicator for Lazarus/Free Pascal. It supports downloads from HTTP and HTTPS sources and the developer can choose if he wants to use Ararat Synapse or FCL/TFPHttpClient in the background. That said it supports OpenSSLv1 and OpenSSLv3.

The Unit has been tested with Windows but should also work on other Systems which are supported by FCL/TFPHttpClient or Synapse (Linux, Mac etc.).

## Synapse vs. TFPHttpClient:

At the moment of writing the official stable version of FPC (3.2.x) only supports SSL v1.1.x which is end of life since January 24, 2024. If you want to get SSL v3 running with the FCL/TFPHttpClient based integration, you need to switch over to the most recent trunk version of FPC (3.3.x) which supports SSL v3. As it is not an option for everyone to switch their development environment over to a (potentially) unstable trunk version you can simply switch to Ararat Synapse by a compiler flag which already officially supports SSL v3.

## How to install?

Simply drop in the lazsimplehttpsget.pas Unit in to your project and take care that FCL or Synapse is linked as a requirement to the project.

![Example Application](https://github.com/dryas/LazSimpleHTTPsGet/blob/main/doc/setrequirements.png)

## Where do I get the requirements?

FCL is shipped as part of the Free Pascal Compiler, so you don't need to install anything. Simply add the FCL as a requirement to your project.
For Synapse you can install it using FpcUpDeluxe or download it from their GitHub account: [Ararat Synapse](https://github.com/geby/synapse "Ararat Synapse")

If you want to use SSL you need to install OpenSSL to your system or store the OpenSSL DLLs in the same directory as your compiled binaries.

## How to switch between FCL and Synapse?

You can simply add a compiler option to your project. By default (so nothing specified) the unit will use FCL. If you want to use Synapse simply add "-dSIMPLEGETSYNAPSE" to your custom compiler options.

![Compiler Options](https://github.com/dryas/LazSimpleHTTPsGet/blob/main/doc/compileroption.png)

## Where to get the OpenSSL DLLs?

As said there are two ways to fulfill the requirements to use SSL. You can install the OpenSSL libraries system wide or distribute the DLLs in the same directory like your compiled binary is stored. Both options have their pro and cons. 

If you don't want to compile the DLLs by yourself, you can, for example, download them from the following location: [FireDaemon OpenSSL](https://kb.firedaemon.com/support/solutions/articles/4000121705-openssl-binary-distributions-for-microsoft-windows "FireDaemon OpenSSL")

## I found a bug or have some ideas to improve it!

I'm not a Pascal expert, even if I use it now for years here and then. If you found a bug or if you want to help and improve the Unit, please fill an Issue or Pull request. Any help is appreciated.