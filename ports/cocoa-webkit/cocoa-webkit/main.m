#import <AppKit/AppKit.h>

#import "NextApplicationDelegate.h"
#import "NextApplication.h"

int main(int argc, const char * argv[])
{
    NSArray *arguments = NSProcessInfo.processInfo.arguments;
    NSLog(@"%@", arguments);

    if ([arguments containsObject:@"-h"] || [arguments containsObject:@"--help"])
    {
        printf("%s\n", [@"Help String!" UTF8String]);
    }
    
    NextApplication *app = [NextApplication sharedApplication];
    NextApplicationDelegate *delegate = [[NextApplicationDelegate alloc] init];
    [app setDelegate:delegate];
    
    if ([arguments containsObject:@"-p"] || [arguments containsObject:@"--port"])
    {
        printf("%s\n", [@"Port Specified" UTF8String]);
    }

    if ([arguments containsObject:@"-s"] || [arguments containsObject:@"--core-socket"])
    {
        printf("%s\n", [@"Core Socket Specified" UTF8String]);
    }
    
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];
    [app run];

    return 0;
}
