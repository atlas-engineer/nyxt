#import <AppKit/AppKit.h>

#import "NextApplicationDelegate.h"
#import "NextApplication.h"
#import "Global.h"

int main(int argc, const char * argv[])
{
    Global *global = [Global sharedInstance];
    NSArray *arguments = NSProcessInfo.processInfo.arguments;

    if ([arguments containsObject:@"-h"] || [arguments containsObject:@"--help"])
    {
        printf("%s\n", [@"Usage:"
                         "\n cocoa-webkit [OPTION...]"
                         "\n Help Options:"
                         "\n   -h, --help                                       Show help options"
                         "\n "
                         "\n Application Options:"
                         "\n  -p, --port=8082                                  Port the XML-RPC server listens to"
                         "\n  -s, --core-socket=http://localhost:8081/RPC2     Socket of the Lisp core"
                        UTF8String]);
    }
    if ([arguments containsObject:@"-p"])
    {
        [global setPort:[arguments objectAtIndex: 1 + [arguments indexOfObject:@"-p"]]];
        NSLog(@"%@", [global port]);
    }
    if ([arguments containsObject:@"--port"])
    {
        [global setPort:[arguments objectAtIndex: 1 + [arguments indexOfObject:@"--port"]]];
        NSLog(@"%@", [global port]);
    }
    if ([arguments containsObject:@"-s"])
    {
        [global setCoreSocket:[arguments objectAtIndex: 1 + [arguments indexOfObject:@"-s"]]];
        NSLog(@"%@", [global coreSocket]);
    }
    if ([arguments containsObject:@"--core-socket"])
    {
        [global setCoreSocket:[arguments objectAtIndex: 1 + [arguments indexOfObject:@"--core-socket"]]];
        NSLog(@"%@", [global coreSocket]);
    }
    
    NSLog(@"Starting server with: Port: %@ Core Socket: %@", [global port], [global coreSocket]);
    
    NextApplication *app = [NextApplication sharedApplication];
    [app setDelegate:[[NextApplicationDelegate alloc] init]];
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];
    [app run];

    return 0;
}
