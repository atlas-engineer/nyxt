#import "XMLRPCResponseDecoder.h"
#import "XMLRPCEventBasedParser.h"

@implementation XMLRPCResponseDecoder

- (id)initWithData: (NSData *)data {
    if (!data) {
        return nil;
    }

    self = [super init];
    if (self) {
        XMLRPCEventBasedParser *parser = [[XMLRPCEventBasedParser alloc] initWithData: data];
        
        if (!parser) {
            return nil;
        }
    
        myBody = [[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding];
        myObject = [parser parse];
        isFault = [parser isFault];
    }
    
    return self;
}

- (BOOL)isFault {
    return isFault;
}

- (NSNumber *)faultCode {
    if (isFault) {
        return [myObject objectForKey: @"faultCode"];
    }
    
    return nil;
}

- (NSString *)faultString {
    if (isFault) {
        return [myObject objectForKey: @"faultString"];
    }
    
    return nil;
}

- (id)object {
    return myObject;
}

- (NSString *)body {
    return myBody;
}

- (NSString *)description {
	NSMutableString	*result = [NSMutableString stringWithCapacity:128];
    
	[result appendFormat:@"[body=%@", myBody];
    
	if (isFault) {
		[result appendFormat:@", fault[%@]='%@'", [self faultCode], [self faultString]];
	} else {
		[result appendFormat:@", object=%@", myObject];
	}
    
	[result appendString:@"]"];
    
	return result;
}

@end
