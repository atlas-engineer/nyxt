#import <AppKit/AppKit.h>

#import "Global.h"
#import "NextApplication.h"
#import "NextApplicationDelegate.h"
#include "argparse.h"

static const char* const usages[] = {
    "cocoa-webkit [options...]",
    NULL,
};

int main(int argc, const char* argv[])
{
    int port = 8082;
    const char* coreSocket = "http://localhost:8081/RPC2";

    struct argparse_option options[] = {
        OPT_HELP(),
        OPT_GROUP("Basic options"),
        OPT_INTEGER('p', "port", &port, "selected integer", NULL, 0, 0),
        OPT_STRING('s', "core-socket", &coreSocket, "XML RPC Address of Lisp Core", NULL, 0, 0),
        OPT_END(),
    };

    struct argparse argparse;
    argparse_init(&argparse, options, usages, 0);
    argparse_describe(&argparse,
        "Coca-Webkit is the platform front end for the Lisp core.", NULL);
    argc = argparse_parse(&argparse, argc, argv);

    Global* global = [Global sharedInstance];
    [global setCoreSocket:[NSString stringWithUTF8String:coreSocket]];
    [global setPort:[@(port) stringValue]];

    NextApplication* app = [NextApplication sharedApplication];
    [app setDelegate:[[NextApplicationDelegate alloc] init]];
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];
    [app run];

    return 0;
}
