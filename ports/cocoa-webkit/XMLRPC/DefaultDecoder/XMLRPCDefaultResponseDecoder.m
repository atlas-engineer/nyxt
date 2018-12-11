#import "XMLRPCDefaultResponseDecoder.h"
#import "XMLRPCEventBasedParserDelegate.h"

@implementation XMLRPCDefaultResponseDecoder

- (id)initWithData:(NSData*)data {
    if (!data) {
        return nil;
    }

    if (self = [self init]) {
        myParser = [[NSXMLParser alloc] initWithData:data];
        myParserDelegate = nil;
        isFault = NO;
    }

    return self;
}

- (id)parse {
    [myParser setDelegate:self];

    [myParser parse];

    if ([myParser parserError]) {
        return nil;
    }

    return [myParserDelegate elementValue];
}

- (void)abortParsing {
    [myParser abortParsing];
}

- (NSError*)parserError {
    return [myParser parserError];
}

- (BOOL)isFault {
    return isFault;
}

@end

@implementation XMLRPCDefaultResponseDecoder (NSXMLParserDelegate)

- (void)parser:(NSXMLParser*)parser
    didStartElement:(NSString*)element
  namespaceURI:(NSString*)namespaceURI
 qualifiedName:(NSString*)qualifiedName
    attributes:(NSDictionary*)attributes {
    if ([element isEqualToString:@"fault"]) {
        isFault = YES;
    } else if ([element isEqualToString:@"value"]) {
        myParserDelegate = [[XMLRPCEventBasedParserDelegate alloc] initWithParent:nil];
        [myParser setDelegate:myParserDelegate];
    }
}

- (void)parser:(NSXMLParser*)parser parseErrorOccurred:(NSError*)parseError {
    [self abortParsing];
}

@end
